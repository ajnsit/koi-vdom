{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Koi.VDom.Types
  ( VDom (..),
    renderWidget,
    Graft,
    runGraft,
    ElemName (..),
    Namespace (..),
  )
where

import Data.Bifunctor (Bifunctor, bimap)
import Data.Maybe (Maybe)

-- | The core virtual-dom tree type, where `a` is the type of attributes,
-- | and `w` is the type of "widgets". Widgets are machines that have complete
-- | control over the lifecycle of some `DOM.Node`.
-- |
-- | The `Grafted` constructor and associated machinery enables `bimap`
-- | fusion using a Coyoneda-like encoding.
data VDom a w
  = Text String
  | Elem ElemName a [VDom a w]
  | Keyed ElemName a [(String, VDom a w)]
  | Widget w
  | Grafted (Graft a w)

instance Functor (VDom a) where
  fmap g (Text a) = Text a
  fmap g (Grafted a) = Grafted (fmap g a)
  fmap g a = Grafted (Graft id g a)

instance Bifunctor VDom where
  bimap f g (Text a) = Text a
  bimap f g (Grafted a) = Grafted (bimap f g a)
  bimap f g a = Grafted (Graft f g a)

-- | Replaces "widgets" in the `VDom` with the ability to turn them into other
-- | `VDom` nodes.
-- |
-- | Using this function will fuse any `Graft`s present in the `VDom`.
renderWidget :: forall a b w x. (a -> b) -> (w -> VDom b x) -> VDom a w -> VDom b x
renderWidget f g = \case
  Text a -> Text a
  Elem n a ch -> Elem n (f a) (map (renderWidget f g) ch)
  Keyed n a ch -> Keyed n (f a) (fmap (fmap (renderWidget f g)) ch)
  Widget w -> g w
  Grafted gaw -> renderWidget f g (runGraft gaw)

data Graft a w = forall a' w'. Graft (a' -> a) (w' -> w) (VDom a' w')

instance Functor (Graft a) where
  fmap g (Graft f' g' a) = Graft f' (g . g') a

instance Bifunctor Graft where
  bimap f g (Graft f' g' a) = Graft (f . f') (g . g') a

runGraft :: forall a' w'. Graft a' w' -> VDom a' w'
runGraft (Graft fa fw v) =
  let go (Text s) = Text s
      go (Elem n a ch) = Elem n (fa a) (map go ch)
      go (Keyed n a ch) = Keyed n (fa a) (fmap (fmap go) ch)
      go (Widget w) = Widget (fw w)
      go (Grafted g) = Grafted (bimap fa fw g)
   in go v

newtype ElemName = ElemName String
  deriving (Eq, Ord)

newtype Namespace = Namespace String
  deriving (Eq, Ord)
