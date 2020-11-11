{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Koi.VDom.Eqq where

-- The idea here is to allow equality
-- across different types

-- This is needed in Thunk implementation where we compare
-- existential variables which may or may not be equal

import Data.Proxy (Proxy (..))
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (makeStableName)

-- It's possible to test for type equality using *closed* type families
-- Typeclasses are not able to do this
type family F a b :: Bool where
  F a a = 'True
  F _ _ = 'False

-- This is the desired class we want to implement
class Eqq a b where
  eqq :: a -> b -> Bool

-- We define a decorated class which takes the equality information from the type level function
-- And then decides how to perform the equality operation
class Eqq' (flag :: Bool) a b where
  eqq' :: Proxy flag -> a -> b -> Bool

-- Then we can dispatch automatically based on the type level function's result
instance (F a b ~ typesAreEq, Eqq' typesAreEq a b) => Eqq a b where
  eqq = eqq' (Proxy :: Proxy typesAreEq)

-- Here's how to compare when the types are equal
instance Eqq' 'True a a where
  eqq' _ a b = unsafePerformIO $ do
    a1 <- makeStableName a
    b1 <- makeStableName b
    return (a1 == b1)

-- Here's how to compare when the types are not equal
-- Comparing unequal types always fails
instance Eqq' 'False a b where
  eqq' _ _ _ = False
