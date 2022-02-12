{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall #-}

-- | This library implements the ST2 monad, a type using GDP (ghosts of departed proofs)
--   to define shared regions of memory between local mutable state threads. This allows
--   us to define a region of the heap in more minute contours, with each state thread
--   having explicit access to regions in memory. This is achieved using the function `runST2`,
--   which in effects lets the user run a computation that makes use of two partially-overlapping
--   memory regions. Within that computation, the user can run sub-computations bound to one or
--   the other memory region. Furthermore, a sub-computation can move any variable that it owns
--   into the common overlap via `share`.
--
--   An example is shown in below, where one sub-computation creates two cells: one
--   private, and the other shared. A second sub-computation has unconstrained access to the
--   shared cell. Yet even though the private reference is also in scope during the second
--   sub-computation, any attempts to access it there will fail to compile.
--
-- >>> :{
-- stSharingDemo :: Bool
-- stSharingDemo = runST2 $ do
--   -- In the "left" memory region, create and return
--   -- two references; one shared, and one not shared.
--   (secret, ref) <- liftL $ do
--     unshared <- newSTRef 42
--     shared   <- share =<< newSTRef 17
--     return (unshared, shared)
--   -- In the "right" memory region, mutate the shared
--   -- reference. If we attempt to access the non-shared
--   -- reference here, the program will not compile.
--   liftR $ do
--     let mine = use (symm ref)
--     x <- readSTRef mine
--     writeSTRef mine (x + 1)
--   -- Back in the "left" memory region, verify that the
--   -- unshared reference still holds its original value.
--   liftL $ do
--     check <- readSTRef secret
--     return (check == 42)
-- :}
module Control.Monad.ST2
  ( -- * ST2 API
    ST(..), STRep
  , fixST
  , liftST

  , runST

  , STRef(..)
  , newSTRef
  , readSTRef
  , writeSTRef

  , type (∩)
  , Common
  , share
  , liftL
  , liftR
  , use
  , symm
  , runST2

    -- * Unsafe API
  , toBaseST
  , fromBaseST

  , STret
  , unsafeInterleaveST, unsafeDupableInterleaveST
  , stToPrim
  , unsafePrimToST
  , unsafeSTToPrim
  , unsafeInlineST
  , stToIO
  , ioToST
  , RealWorld
  ) where

import           Control.Applicative (Applicative(pure, (*>), (<*>), liftA2))
import           Control.Exception.Base (catch, throwIO, NonTermination(..), BlockedIndefinitelyOnMVar(..))
import           Control.Monad (Monad(return, (>>=), (>>)), ap, liftM2)
import           Control.Monad.Primitive (PrimMonad(primitive, PrimState), PrimBase(internal), primToPrim, unsafePrimToPrim, unsafeInlinePrim)
import qualified Control.Monad.ST as BaseST
import           Data.Eq (Eq((==)))
import           Data.Function (($), (.))
import           Data.Functor (Functor(fmap))
import           Data.Kind (Type)
#if !(MIN_VERSION_base(4,11,0))
import           Data.Monoid (Monoid(mempty, mappend))
#else
import           Data.Monoid (Monoid(mempty))
#endif
import           Data.Semigroup (Semigroup((<>)))
import           GHC.IO (IO(IO),unsafeDupableInterleaveIO)
import           GHC.MVar (readMVar, putMVar, newEmptyMVar)
import           GHC.Exts (State#, unsafeCoerce#, MutVar#, newMutVar#, readMutVar#, writeMutVar#, sameMutVar#, RealWorld, noDuplicate#, RuntimeRep, TYPE, Any, isTrue#)
import           GHC.Show (Show(showsPrec, showList), showString, showList__)
import           Theory.Named (type (~~))
import           Unsafe.Coerce (unsafeCoerce)
import qualified GHC.Exts as GHCExts

-- | Convert an ST2 to an ST
toBaseST :: ST s a -> BaseST.ST s a
{-# INLINE toBaseST #-}
toBaseST = unsafeCoerce

-- | Convert an ST to an ST2
fromBaseST :: BaseST.ST s a -> ST s a
{-# INLINE fromBaseST #-}
fromBaseST = unsafeCoerce

-- The state-transformer monad proper.  By default the monad is strict;
-- too many people got bit by space leaks when it was lazy.

-- | The strict state-transformer monad.
-- A computation of type @'ST' s a@ transforms an internal state indexed
-- by @s@, and returns a value of type @a@.
-- The @s@ parameter is either
--
-- * an uninstantiated type variable (inside invocations of 'runST'), or
--
-- * 'RealWorld' (inside invocations of 'Control.Monad.ST.stToIO').
--
-- It serves to keep the internal states of different invocations
-- of 'runST' separate from each other and from invocations of
-- 'Control.Monad.ST.stToIO'.
--
-- The '>>=' and '>>' operations are strict in the state (though not in
-- values stored in the state).  For example,
--
-- @'runST' (writeSTRef _|_ v >>= f) = _|_@
newtype ST (s :: Type) a = ST (STRep (Any ~~ s) a)

-- | Convenience type alias for expressing ST computations
--   more succintly.
type STRep s a = State# s -> (# State# s, a #)

instance PrimMonad (ST s) where
  type PrimState (ST s) = s
  primitive = ST . repToAny#
  {-# INLINE primitive #-}

instance PrimBase (ST s) where
  internal (ST p) = repFromAny# p
  {-# INLINE internal #-}

-- | A simple product type containing both the state thread and value inside of the 'ST'
data STret s a = STret (State# s) a

-- | 'liftST' is useful when we want a lifted result from an 'ST' computation. See
--   'fixST' below.
liftST :: ST s a -> State# s -> STret s a
liftST (ST m) = \s -> case m (unsafeCoerce# s) of (# s', r #) -> STret (unsafeCoerce# s') r

noDuplicateST :: ST s ()
{-# INLINE noDuplicateST #-}
noDuplicateST = ST $ \s -> (# noDuplicate# s, () #)

-- | 'unsafeInterleaveST' allows an 'ST' computation to be deferred
-- lazily.  When passed a value of type @ST a@, the 'ST' computation will
-- only be performed when the value of the @a@ is demanded.
{-# INLINE unsafeInterleaveST #-}
unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST m = unsafeDupableInterleaveST (noDuplicateST >> m)

-- | 'unsafeDupableInterleaveST' allows an 'ST' computation to be deferred
-- lazily.  When passed a value of type @ST a@, the 'ST' computation will
-- only be performed when the value of the @a@ is demanded.
--
-- The computation may be performed multiple times by different threads,
-- possibly at the same time. To prevent this, use 'unsafeInterleaveST' instead.
{-# NOINLINE unsafeDupableInterleaveST #-}
-- See Note [unsafeDupableInterleaveIO should not be inlined]
-- in GHC.IO.Unsafe
unsafeDupableInterleaveST :: ST s a -> ST s a
unsafeDupableInterleaveST (ST m) = ST ( \ s ->
    let
        r = case m s of (# _, res #) -> res
    in
    (# s, r #)
  )

-- | Embed a strict state transformer in an 'IO'
-- action.  The 'RealWorld' parameter indicates that the internal state
-- used by the 'ST' computation is a special one supplied by the 'IO'
-- monad, and thus distinct from those used by invocations of 'runST'.
stToIO        :: ST RealWorld a -> IO a
stToIO (ST m) = IO (unsafeCoerce m)

-- | Convert an 'IO' action into an 'ST' action. The type of the result
-- is constrained to use a 'RealWorld' state, and therefore the result cannot
-- be passed to 'runST'.
ioToST        :: IO a -> ST RealWorld a
ioToST (IO m) = ST (unsafeCoerce m)

-- | Convert an 'IO' action to an 'ST' action.
-- This relies on 'IO' and 'ST' having the same representation modulo the
-- constraint on the type of the state.
unsafeIOToST        :: IO a -> ST s a
unsafeIOToST (IO io) = ST $ \ s -> (unsafeCoerce# io) s

-- | Convert an 'ST' action to an 'IO' action.
-- This relies on 'IO' and 'ST' having the same representation modulo the
-- constraint on the type of the state.
--
-- For an example demonstrating why this is unsafe, see
-- https://mail.haskell.org/pipermail/haskell-cafe/2009-April/060719.html
unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO (ST m) = IO (unsafeCoerce# m)

-- | Convert an 'ST' action to a 'PrimMonad'.
stToPrim :: PrimMonad m => ST (PrimState m) a -> m a
{-# INLINE stToPrim #-}
stToPrim = primToPrim

-- | Convert any 'PrimBase' to 'ST' with an arbitrary state token. This operation is highly unsafe!
unsafePrimToST :: PrimBase m => m a -> ST s a
{-# INLINE unsafePrimToST #-}
unsafePrimToST = unsafePrimToPrim

-- | Convert an 'ST' action with an arbitrary state token to any 'PrimMonad'. This operation is highly unsafe!
unsafeSTToPrim :: PrimBase m => ST s a -> m a
{-# INLINE unsafeSTToPrim #-}
unsafeSTToPrim = unsafePrimToPrim

-- | Extract the value out of the 'ST' computation. Compare this to 'runST'; in this case, the rest of the program
--   is permitted to reference the state thread 's'. This operation is highly unsafe!
unsafeInlineST :: ST s a -> a
{-# INLINE unsafeInlineST #-}
unsafeInlineST = unsafeInlinePrim

-- | A value of type @STRef s a@ is a mutable variable in state thread @s@,
--   containing a value of type @a@
data STRef s a = STRef (MutVar# s a)

-- |Build a new 'STRef' in the current state thread
newSTRef :: a -> ST s (STRef s a)
newSTRef init = ST $ \s1# ->
    case newMutVar# init (rwFromAny# s1#) of { (# s2#, var# #) ->
      (# (unsafeCoerce# s2#), STRef var# #) }

-- |Read the value of an 'STRef'
readSTRef :: STRef s a -> ST s a
readSTRef (STRef var#) = ST $ \s1# -> rwTupleToAny# (readMutVar# var# (rwFromAny# s1#))

-- | Write a new value into an 'STRef'
writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef (STRef var#) val = ST $ \s1# ->
  case writeMutVar# var# val (rwFromAny# s1#) of
    s2# -> (# (rwToAny# s2#), () #)

-- Just pointer equality on mutable references:
instance Eq (STRef s a) where
  STRef v1# == STRef v2# = isTrue# (sameMutVar# v1# v2#)

instance Functor (ST s) where
  fmap f (ST m) = ST $ \ s ->
    case (m s) of { (# new_s, r #) ->
      (# new_s, f r #) }

instance Applicative (ST s) where
  {-# INLINE pure #-}
  {-# INLINE (*>) #-}
  pure x = ST (\ s -> (# s, x #))
  m *> k = m >>= \ _ -> k
  (<*>) = ap
  liftA2 = liftM2

instance Monad (ST s) where
    {-# INLINE (>>=)  #-}
    (>>) = (*>)
    (ST m) >>= k
      = ST (\ s ->
        case (m s) of { (# new_s, r #) ->
        case (k r) of { ST k2 ->
        (k2 new_s) }})

instance Semigroup a => Semigroup (ST s a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (ST s a) where
  mempty = pure mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = liftA2 mappend
#endif

instance Show (ST s a) where
  showsPrec _ _ = showString "<<ST action>>"
  showList      = showList__ (showsPrec 0)

-- | A pretty type alias for 'Common'.
type s ∩ s' = Common s s'

-- | A type that shows that the state threads s and s' refer to a heap
--   region that overlaps in some way such that s and s' can both access
--   the overlap, while maintaining privacy in their own heap regions.
data Common s s'

-- | Move a variable that you own into a region with
--   common overlap.
share :: STRef s a -> ST s (STRef (Common s s') a)
{-# INLINE share #-}
share = return . unsafeCoerce

-- | Lift an 'ST' computation into a context with another heap region
liftL :: ST s a -> ST (Common s s') a
{-# INLINE liftL #-}
liftL = unsafeCoerce

-- | Lift an 'ST' computation into a context with another heap region
liftR :: ST s' a -> ST (Common s s') a
{-# INLINE liftR #-}
liftR = unsafeCoerce

-- | Given proof that one has access to the heap regions s and s',
--   yield an STRef to the region s.
use :: STRef (Common s s') a -> STRef s a
{-# INLINE use #-}
use = unsafeCoerce

-- | Given proof that one has access to the heap regions s and s',
--   yield an 'STRef' that swaps the order of the regions.
symm :: STRef (Common s s') a -> STRef (Common s' s) a
{-# INLINE symm #-}
symm = unsafeCoerce

-- | Return the value computed by a state transformer computation
--   over a shared heap. The @forall@ ensures that the internal state(s)
--   used by the 'ST' computation is inaccessible to the rest of the program.
runST2 :: (forall s s'. ST (Common s s') a) -> a
{-# INLINE runST2 #-}
runST2 (ST st_rep) = case runRegion# st_rep of (# _, a #) -> a

-- | Return the value computed by a state transformer computation.
--   The @forall@ ensures that the internal state used by the 'ST'
--   computation is inaccessible to the rest of the program.
runST :: (forall s. ST s a) -> a
{-# INLINE runST #-}
runST (ST st_rep) = case runRegion# st_rep of (# _, a #) -> a

-- | Allow the result of a state transformer computation to be used (lazily)
-- inside the computation.
--
-- Note that if @f@ is strict, @'fixST' f = _|_@.
fixST :: (a -> ST s a) -> ST s a
-- See Note [fixST]
fixST k = unsafeIOToST $ do
    m <- newEmptyMVar
    ans <- unsafeDupableInterleaveIO
             (readMVar m `catch` \BlockedIndefinitelyOnMVar ->
                                    throwIO NonTermination)
    result <- unsafeSTToIO (k ans)
    putMVar m result
    return result

{- Note [fixST]
   ~~~~~~~~~~~~
For many years, we implemented fixST much like a pure fixpoint,
using liftST:
  fixST :: (a -> ST s a) -> ST s a
  fixST k = ST $ \ s ->
      let ans       = liftST (k r) s
          STret _ r = ans
      in
      case ans of STret s' x -> (# s', x #)
We knew that lazy blackholing could cause the computation to be re-run if the
result was demanded strictly, but we thought that would be okay in the case of
ST. However, that is not the case (see Trac #15349). Notably, the first time
the computation is executed, it may mutate variables that cause it to behave
*differently* the second time it's run. That may allow it to terminate when it
should not. More frighteningly, Arseniy Alekseyev produced a somewhat contrived
example ( https://mail.haskell.org/pipermail/libraries/2018-July/028889.html )
demonstrating that it can break reasonable assumptions in "trustworthy" code,
causing a memory safety violation. So now we implement fixST much like we do
fixIO. See also the implementation notes for fixIO. Simon Marlow wondered
whether we could get away with an IORef instead of an MVar. I believe we
cannot. The function passed to fixST may spark a parallel computation that
demands the final result. Such a computation should block until the final
result is available.
-}

runRegion# :: forall (r :: RuntimeRep) (o :: TYPE r) s.
           (State# (Any ~~ s) -> o) -> o
runRegion# m = GHCExts.runRW# (unsafeCoerce# m) -- m = m (rwToAny# realWorld#)
{-# INLINE runRegion# #-}

{- Note [runRegion#]
   ~~~~~~~~~~~~~~~~~
Originally, `runRegion#` was defined quite similarly to runRW#:
  runRegion# :: forall (r :: RuntimeRep) (o :: TYPE r) s.
    (State# (Any ~~ s) -> o) -> o
  runRegion# m = m (rwToAny# realWorld#)

But this definition is extremely brittle under optimisations! You should never
define a function that performs as `runRW#` does without defining it in _terms_
of `runRW#`. You can get semantically undesirable floating - runRW# is treated
specially by Core and inlined only very late in compilation, after floating is
complete. Below, I will inline "Note [runRW magic]" which is written in ghc's
compiler/coreSyn/CorePrep.hs:

Some definitions, for instance @runST@, must have careful control over float out
of the bindings in their body. Consider this use of @runST@,

    f x = runST ( \ s -> let (a, s')  = newArray# 100 [] s
                             (_, s'') = fill_in_array_or_something a x s'
                         in freezeArray# a s'' )

If we inline @runST@, we'll get:

    f x = let (a, s')  = newArray# 100 [] realWorld#{-NB-}
              (_, s'') = fill_in_array_or_something a x s'
          in freezeArray# a s''

And now if we allow the @newArray#@ binding to float out to become a CAF,
we end up with a result that is totally and utterly wrong:

    f = let (a, s')  = newArray# 100 [] realWorld#{-NB-} -- YIKES!!!
        in \ x ->
            let (_, s'') = fill_in_array_or_something a x s'
            in freezeArray# a s''

All calls to @f@ will share a {\em single} array! Clearly this is nonsense and
must be prevented.

This is what @runRW#@ gives us: by being inlined extremely late in the
optimization (right before lowering to STG, in CorePrep), we can ensure that
no further floating will occur. This allows us to safely inline things like
@runST@, which are otherwise needlessly expensive (see #10678 and #5916).

'runRW' is defined (for historical reasons) in GHC.Magic, with a NOINLINE
pragma.  It is levity-polymorphic.

    runRW# :: forall (r1 :: RuntimeRep). (o :: TYPE r)
           => (State# RealWorld -> (# State# RealWorld, o #))
                              -> (# State# RealWorld, o #)

It needs no special treatment in GHC except this special inlining
in CorePrep (and in ByteCodeGen).

-}

rwToAny# :: forall s s'. State# s' -> State# (Any ~~ s)
rwToAny# x# = unsafeCoerce# x#
{-# INLINE rwToAny# #-}

rwFromAny# :: forall s s'. State# (Any ~~ s) -> State# s'
rwFromAny# x# = unsafeCoerce# x#
{-# INLINE rwFromAny# #-}

--rwTupleFromAny# :: forall s a. (# State# (Any ~~ s), a #) -> (# State# s, a #)
--rwTupleFromAny# (# x, a #) = (# unsafeCoerce# x, a #)

rwTupleToAny# :: forall s a. (# State# s, a #) -> (# State# (Any ~~ s), a #)
rwTupleToAny# (# x, a #) = (# unsafeCoerce# x, a #)
{-# INLINE rwTupleToAny# #-}

repToAny# :: (State# s -> (# State# s, a #)) -> STRep (Any ~~ s) a
repToAny# = unsafeCoerce#
{-# INLINE repToAny# #-}

repFromAny# :: STRep (Any ~~ s) a -> (State# s -> (# State# s, a #))
repFromAny# = unsafeCoerce#
{-# INLINE repFromAny# #-}
