{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UnboxedTuples     #-}

module ST
  (
  ) where

--import GHC.Magic
import Control.Applicative (Applicative(pure, (*>), (<*>), liftA2))
import Data.Functor (Functor(fmap))
import Data.Function (($))
import Data.Semigroup (Semigroup((<>)))
import Data.Monoid (Monoid(mempty, mappend))
import Control.Monad (Monad(return, (>>=), (>>) ), ap, liftM2)
import GHC.Prim (State#, RealWorld, realWorld#, unsafeCoerce#)
import GHC.Types (RuntimeRep, TYPE, Any)
import GHC.Err (undefined)
import Theory.Named
import GHC.Show

newtype ST s a = ST (STRep (Any ~~ s) a)
type STRep s a = State# s -> (# State# s, a #)

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

{-# INLINE runST #-}
runST :: (forall s. ST s a) -> a
runST (ST st_rep) = case runRegion# st_rep of (# _, a #) -> a

runRegion# :: forall (r :: RuntimeRep) (o :: TYPE r) s.
           (State# (Any ~~ s) -> o) -> o
runRegion# m = m (rwToAny# realWorld#)

rwToAny# :: forall s. State# RealWorld -> State# (Any ~~ s)
rwToAny# x# = unsafeCoerce# x#
