{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnboxedTuples       #-}

module ST
  (
  ) where

import Control.Applicative (Applicative(pure, (*>), (<*>), liftA2))
import Control.Monad (Monad(return, (>>=), (>>)), (=<<), ap, liftM2)
import Data.Bool (Bool(True,False))
import Data.Coerce (coerce)
import Data.Eq (Eq((==)))
import Data.Function (($), (.))
import Data.Functor (Functor(fmap))
import Data.Monoid (Monoid(mempty, mappend))
import Data.Semigroup (Semigroup((<>)))
import GHC.Err (undefined)
import GHC.Prim (State#, RealWorld, realWorld#, unsafeCoerce#, MutVar#, newMutVar#, readMutVar#, writeMutVar#, sameMutVar#)
import GHC.Num ((+))
import GHC.Show
import GHC.Types (RuntimeRep, TYPE, Any, isTrue#, Int)
import Theory.Named
import Unsafe.Coerce (unsafeCoerce)

newtype ST s a = ST (STRep (Any ~~ s) a)
type STRep s a = State# s -> (# State# s, a #)

data STRef s a = STRef (MutVar# s a)
-- ^ a value of type @STRef s a@ is a mutable variable in state thread @s@,
-- containing a value of type @a@

-- |Build a new 'STRef' in the current state thread
newSTRef :: a -> ST s (STRef s a)
newSTRef init = ST $ \s1# ->
    case newMutVar# init (rwFromAny# s1#) of { (# s2#, var# #) ->
      (# (unsafeCoerce# s2#), STRef var# #) }

-- |Read the value of an 'STRef'
readSTRef :: STRef s a -> ST s a
readSTRef (STRef var#) = ST $ \s1# -> rwTupleToAny# (readMutVar# var# (rwFromAny# s1#))

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
  mappend = liftA2 mappend

instance Show (ST s a) where
  showsPrec _ _ = showString "<<ST action>>"
  showList      = showList__ (showsPrec 0)

data Common s s'

share :: STRef s a -> ST s (STRef (Common s s') a)
share = return . unsafeCoerce

liftL :: ST s a -> ST (Common s s') a
liftL = unsafeCoerce

liftR :: ST s' a -> ST (Common s s') a
liftR = unsafeCoerce

use :: STRef (Common s s') a -> STRef s a
use = unsafeCoerce

symm :: STRef (Common s s') a -> STRef (Common s' s) a
symm = unsafeCoerce

runST2 :: (forall s s'. ST (Common s s') a) -> a
runST2 (ST st_rep) = case runRegion# st_rep of (# _, a #) -> a

stSharingDemo :: Bool
stSharingDemo = runST2 $ do
  (secret, ref) :: (STRef s Int, STRef (Common s s') Int) <- liftL $ do
    unshared :: STRef s Int <- newSTRef 42
    shared :: STRef (Common s s') Int <- share =<< newSTRef 17
    return (unshared, shared)

  liftR $ do
    let mine = use (symm ref)
    x <- readSTRef mine
    writeSTRef mine (x + 1)

  liftL $ do
    check <- readSTRef secret
    return (check == 42)

{-# INLINE runST #-}
runST :: (forall s. ST s a) -> a
runST (ST st_rep) = case runRegion# st_rep of (# _, a #) -> a

runRegion# :: forall (r :: RuntimeRep) (o :: TYPE r) s.
           (State# (Any ~~ s) -> o) -> o
runRegion# m = m (rwToAny# realWorld#)

rwToAny# :: forall s s'. State# s' -> State# (Any ~~ s)
rwToAny# x# = unsafeCoerce# x#

rwFromAny# :: forall s s'. State# (Any ~~ s) -> State# s'
rwFromAny# x# = unsafeCoerce# x#

rwTupleFromAny# :: forall s a. (# State# (Any ~~ s), a #) -> (# State# s, a #)
rwTupleFromAny# (# x, a #) = (# unsafeCoerce# x, a #)

rwTupleToAny# :: forall s a. (# State# s, a #) -> (# State# (Any ~~ s), a #)
rwTupleToAny# (# x, a #) = (# unsafeCoerce# x, a #)
