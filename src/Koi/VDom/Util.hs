{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}

module Koi.VDom.Util where

--   ( refEq,
--     Object (..),
--     forE,
--     diffWithIxE,
--     diffWithKeyAndIxE,
--     forEachE,
--     strMapWithIxE,
--     forInE,
--   )

import Data.Foldable (traverse_)
import qualified Data.IORef as IORef
import Data.List.Index
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (makeStableName)

refEq :: forall a. a -> a -> IO Bool
refEq a b = do
  x <- makeStableName a
  y <- makeStableName b
  return (x == y)

unsafeRefEq :: forall a. a -> a -> Bool
unsafeRefEq a b = unsafePerformIO $ refEq a b

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

-- Our own mutable strmap implementation
type Object a = IORef.IORef (Map.Map String a)

unsafeEmpty :: IO (Object a)
unsafeEmpty = IORef.newIORef (Map.empty)

unsafeNew :: Map.Map String a -> IO (Object a)
unsafeNew = IORef.newIORef

pokeMutMap :: String -> a -> Object a -> IO ()
pokeMutMap = unsafeSetAny

unsafeGetMaybe :: String -> Object a -> IO (Maybe a)
unsafeGetMaybe k o = do
  m <- IORef.readIORef o
  pure (Map.lookup k m)

unsafeGet :: String -> Object a -> IO a
unsafeGet k o = do
  m <- IORef.readIORef o
  pure $ fromMaybe (error $ "unsafeGet: " ++ k) $ Map.lookup k m

unsafeSetAny :: String -> a -> Object a -> IO ()
unsafeSetAny k v o = IORef.modifyIORef o (Map.insert k v)

unsafeDelete :: String -> Object a -> IO ()
unsafeDelete k o = IORef.modifyIORef o (Map.delete k)

forE :: forall a b. [a] -> (Int -> a -> IO b) -> IO [b]
forE arr f = sequence $ imap f arr

diffWithIxE ::
  forall b c d.
  [b] ->
  [c] ->
  (Int -> b -> c -> IO d) ->
  (Int -> b -> IO ()) ->
  (Int -> c -> IO d) ->
  IO [d]
diffWithIxE arr brr f1 f2 f3 = go 0 arr brr
  where
    go i (a : as) (b : bs) = do
      x <- f1 i a b
      xs <- go (i + 1) as bs
      return (x : xs)
    go i (a : as) [] = do
      f2 i a
      go (i + 1) as []
    go i [] (b : bs) = do
      x <- f3 i b
      xs <- go (i + 1) [] bs
      return (x : xs)
    go _ _ _ = return []

forEachE ::
  forall a.
  [a] ->
  (a -> IO ()) ->
  IO ()
forEachE arr f = traverse_ f arr

strMapWithIxE ::
  forall a b.
  [a] ->
  (a -> String) ->
  (String -> Int -> a -> IO b) ->
  IO (Object b)
strMapWithIxE as fk f = do
  tups <- izipWithM fun (map fk as) as
  unsafeNew $ Map.fromList tups
  where
    fun i s a = do
      b <- f s i a
      return (s, b)

forInE ::
  forall a.
  Object a ->
  (String -> a -> IO ()) ->
  IO ()
forInE o f = do
  m <- IORef.readIORef o
  Map.foldMapWithKey f m

diffWithKeyAndIxE ::
  forall a b c d.
  Object a ->
  [b] ->
  (b -> String) ->
  (String -> Int -> a -> b -> IO c) ->
  (String -> a -> IO d) ->
  (String -> Int -> b -> IO c) ->
  IO (Object c)
diffWithKeyAndIxE o1 as fk f1 f2 f3 = do
  m1 <- IORef.readIORef o1
  m2 <- ifoldr (fun m1) (pure Map.empty) as
  let diff = Map.difference m1 m2
  Map.foldlWithKey fun2 (return ()) diff
  unsafeNew m2
  where
    fun2 eff k b = do
      eff
      f2 k b
      return ()
    fun m1 i a mo = do
      o <- mo
      let k = fk a
      v <- case Map.lookup k m1 of
        Nothing -> f3 k i a
        Just x -> f1 k i x a
      return (Map.insert k v o)

{-

foreign import replicateE
  ∷ ∀ a
  . EFn.EffectFn2
      Int
      (Effect a)
      Unit

foreign import diffWithIxE
  ∷ ∀ b c d
  . EFn.EffectFn5
      (Array b)
      (Array c)
      (EFn.EffectFn3 Int b c d)
      (EFn.EffectFn2 Int b Unit)
      (EFn.EffectFn2 Int c d)
      (Array d)

foreign import refEq
  ∷ ∀ a b. Fn.Fn2 a b Boolean

foreign import data JsUndefined ∷ Type

foreign import jsUndefined ∷ JsUndefined

-}
