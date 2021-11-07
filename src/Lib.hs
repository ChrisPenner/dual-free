{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib where
import Control.Monad.Free
import Data.Functor.Compose
import Control.Comonad.Cofree
import Data.Bifunctor
import Data.Bitraversable
import Data.Bifoldable
import Control.Applicative
import Data.Function
import Control.Comonad

newtype DualFree f a b = DualFree (Free (Compose ((,) a) f) b)
  deriving stock (Functor, Foldable, Traversable)
  deriving newtype (Applicative, Monad)

instance Functor f => Bifunctor (DualFree f) where
  bimap l r (DualFree x) =
    x
    & fmap r
    & hoistFree ((\(Compose (a, fx)) -> Compose (l a, fx)))
    & DualFree

instance Foldable f => Bifoldable (DualFree f) where
  bifoldMap l r (DualFree x) = go x
    where
      go x = case x of
               Pure b -> r b
               Free (Compose (a, fx)) -> l a <> foldMap go fx


instance Traversable f => Bitraversable (DualFree f) where
  bitraverse f g (DualFree x) = DualFree <$> go x
    where
      go x =
        case x of
          Pure b -> Pure <$> g b
          Free (Compose (a, fx)) -> Free . Compose <$> liftA2 (,) (f a) (traverse go fx)

newtype DualCofree f a b = DualCofree (Cofree (Compose (Either a) f) b)
  deriving stock (Functor, Foldable, Traversable)
  deriving newtype (Applicative, Monad)

instance Functor f => Comonad (DualCofree f a) where
  extract (DualCofree x) = extract x
  duplicate (DualCofree x) = DualCofree (DualCofree <$> duplicate x)

instance Functor f => Bifunctor (DualCofree f) where
  bimap l r (DualCofree x) =
    x
    & fmap r
    & hoistCofree ((\(Compose e) -> Compose (first l e)))
    & DualCofree

instance Foldable f => Bifoldable (DualCofree f) where
  bifoldMap l r (DualCofree x) = go x
    where
      go (b :< Compose e) = r b <> either l (foldMap go) e

instance Traversable f => Bitraversable (DualCofree f) where
  bitraverse f g (DualCofree x) = DualCofree <$> go x
    where
      go (b :< Compose e) =
        case e of
          Left a -> do
            b' <- g b
            a' <- f a
            pure (b' :< Compose (Left a'))
          Right fx -> do
            b' <- (g b)
            fx' <- traverse go fx
            pure (b' :< Compose (Right fx'))
