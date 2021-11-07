{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Comonad.DualCofree where
import Control.Comonad.Cofree
import Data.Functor.Compose
import Control.Comonad
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Function

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
