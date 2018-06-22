{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Weverything #-}

module Control.Monad.ST2
  ( ST(..)
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
  
  , toBaseST
  , fromBaseST
  
  , stToPrim
  , unsafePrimToST
  , unsafeSTToPrim
  , unsafeInlineST
  ) where

import           Control.Applicative (Applicative(pure, (*>), (<*>), liftA2))
import           Control.Monad (Monad(return, (>>=), (>>)), ap, liftM2)
import           Control.Monad.Primitive (PrimMonad(primitive, PrimState), PrimBase(internal), primToPrim, unsafePrimToPrim, unsafeInlinePrim)
import qualified Control.Monad.ST as BaseST
import           Data.Eq (Eq((==)))
import           Data.Function (($), (.))
import           Data.Functor (Functor(fmap))
#if !(MIN_VERSION_base(4,11,0))
import           Data.Monoid (Monoid(mempty, mappend))
#else
import           Data.Monoid (Monoid(mempty))
#endif
import           Data.Semigroup (Semigroup((<>)))
import           GHC.Prim (State#, realWorld#, unsafeCoerce#, MutVar#, newMutVar#, readMutVar#, writeMutVar#, sameMutVar#)
import           GHC.Show (Show(showsPrec, showList), showString, showList__)
import           GHC.Types (RuntimeRep, TYPE, Any, isTrue#)
import           Theory.Named (type (~~))
import           Unsafe.Coerce (unsafeCoerce)

toBaseST :: ST s a -> BaseST.ST s a
{-# INLINE toBaseST #-}
toBaseST = unsafeCoerce

fromBaseST :: BaseST.ST s a -> ST s a
{-# INLINE fromBaseST #-}
fromBaseST = unsafeCoerce

newtype ST s a = ST (STRep (Any ~~ s) a)
type STRep s a = State# s -> (# State# s, a #)

instance PrimMonad (ST s) where
  type PrimState (ST s) = s
  primitive = ST . repToAny#
  {-# INLINE primitive #-}

instance PrimBase (ST s) where
  internal (ST p) = repFromAny# p
  {-# INLINE internal #-}

stToPrim :: PrimMonad m => ST (PrimState m) a -> m a
{-# INLINE stToPrim #-}
stToPrim = primToPrim

unsafePrimToST :: PrimBase m => m a -> ST s a
{-# INLINE unsafePrimToST #-}
unsafePrimToST = unsafePrimToPrim

unsafeSTToPrim :: PrimBase m => ST s a -> m a
{-# INLINE unsafeSTToPrim #-}
unsafeSTToPrim = unsafePrimToPrim

unsafeInlineST :: ST s a -> a
{-# INLINE unsafeInlineST #-}
unsafeInlineST = unsafeInlinePrim

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
#if !(MIN_VERSION_base(4,11,0))
  mappend = liftA2 mappend
#endif

instance Show (ST s a) where
  showsPrec _ _ = showString "<<ST action>>"
  showList      = showList__ (showsPrec 0)

type s ∩ s' = Common s s'

data Common s s'

share :: STRef s a -> ST s (STRef (Common s s') a)
{-# INLINE share #-}
share = return . unsafeCoerce

liftL :: ST s a -> ST (Common s s') a
{-# INLINE liftL #-}
liftL = unsafeCoerce

liftR :: ST s' a -> ST (Common s s') a
{-# INLINE liftR #-}
liftR = unsafeCoerce

use :: STRef (Common s s') a -> STRef s a
{-# INLINE use #-}
use = unsafeCoerce

symm :: STRef (Common s s') a -> STRef (Common s' s) a
{-# INLINE symm #-}
symm = unsafeCoerce

runST2 :: (forall s s'. ST (Common s s') a) -> a
{-# INLINE runST2 #-}
runST2 (ST st_rep) = case runRegion# st_rep of (# _, a #) -> a

{-# INLINE runST #-}
runST :: (forall s. ST s a) -> a
runST (ST st_rep) = case runRegion# st_rep of (# _, a #) -> a

runRegion# :: forall (r :: RuntimeRep) (o :: TYPE r) s.
           (State# (Any ~~ s) -> o) -> o
runRegion# m = m (rwToAny# realWorld#)
{-# INLINE runRegion# #-}

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

