{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Koi.VDom.Machine
  ( Machine,
    Step (..),
    extract,
    step,
    halt,
  )
where

type Machine a b = a -> IO (Step a b)

data Step a b = forall s. Step b s (s -> a -> IO (Step a b)) (s -> IO ())

-- | Returns the output value of a `Step`.
extract :: forall a b. Step a b -> b
extract (Step x _ _ _) = x

-- | Runs the next step.
step :: forall a b. (Step a b) -> a -> IO (Step a b)
step (Step _ s k _) = k s

-- | Runs the finalizer associated with a `Step`
halt :: forall a b. Step a b -> IO ()
halt (Step _ s _ k) = k s
