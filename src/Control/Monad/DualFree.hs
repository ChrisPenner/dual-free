{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.DualFree where
import Control.Monad.Free
import Data.Functor.Compose
import Control.Comonad.Cofree
import Data.Bifunctor
import Data.Bitraversable
import Data.Bifoldable
import Control.Applicative
import Data.Function

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

