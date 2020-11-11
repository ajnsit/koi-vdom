{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Koi.VDom.Thunk
  ( Thunk,
    buildThunk,
    runThunk,
    thunked,
    thunk1,
    thunk2,
    thunk3,
  )
where

import Koi.VDom.HostConfig (HostConfig)
import Koi.VDom.Machine as M
  ( Machine,
    Step (..),
    extract,
    halt,
    step,
  )
import Koi.VDom.Util as Util (fst3, snd3, thd3, unsafeRefEq)
import Unsafe.Coerce (unsafeCoerce)
import Prelude

data ThunkId

data ThunkArg

unsafeThunkId :: forall a. a -> ThunkId
unsafeThunkId = unsafeCoerce

data Thunk a = Thunk ThunkId (ThunkArg -> ThunkArg -> Bool) (ThunkArg -> a) ThunkArg

instance Functor Thunk where
  fmap f (Thunk a b c d) = Thunk a b (f . c) d

thunk :: forall a b. ThunkId -> (b -> b -> Bool) -> (b -> a) -> b -> Thunk a
thunk tid eqFn f b =
  Thunk
    tid
    (unsafeCoerce eqFn :: ThunkArg -> ThunkArg -> Bool)
    (unsafeCoerce f :: ThunkArg -> a)
    (unsafeCoerce b :: ThunkArg)

thunked :: forall a b. (b -> b -> Bool) -> (b -> a) -> b -> Thunk a
thunked eqFn f b = thunk (unsafeThunkId f) eqFn f b

thunk1 :: forall a b. (b -> a) -> b -> Thunk a
thunk1 f b = thunk (unsafeThunkId f) Util.unsafeRefEq f b

thunk2 :: forall a b c. (b -> c -> a) -> b -> c -> Thunk a
thunk2 f a b = thunk (unsafeThunkId f) eqFn (\(x1, x2) -> f x1 x2) (a, b)
  where
    eqFn a' b' = Util.unsafeRefEq (fst a') (fst b') && Util.unsafeRefEq (snd a') (snd b')

thunk3 :: forall a b c d. (a -> b -> c -> d) -> a -> b -> c -> Thunk d
thunk3 f a b c = thunk (unsafeThunkId f) eqFn (\(x1, x2, x3) -> f x1 x2 x3) (a, b, c)
  where
    eqFn a b =
      Util.unsafeRefEq (fst3 a) (fst3 b)
        && Util.unsafeRefEq (snd3 a) (snd3 b)
        && Util.unsafeRefEq (thd3 a) (thd3 b)

runThunk :: forall a. Thunk a -> a
runThunk (Thunk _ _ render arg) = render arg

unsafeEqThunk :: forall a. Thunk a -> Thunk a -> Bool
unsafeEqThunk (Thunk a1 b1 _ d1) (Thunk a2 b2 _ d2) =
  Util.unsafeRefEq a1 a2
    && Util.unsafeRefEq b1 b2
    && b1 d1 d2

data ThunkState node x = ThunkState
  { getThunk :: Thunk x,
    getStep :: M.Step x node
  }

buildThunk ::
  forall x node.
  HostConfig node =>
  (x -> IO (M.Machine x node)) ->
  M.Machine (Thunk x) node
buildThunk mkMachine = renderThunk
  where
    renderThunk :: M.Machine (Thunk x) node
    renderThunk t = do
      let x = runThunk t
      machine <- mkMachine x
      step <- machine x
      pure $ M.Step (M.extract step) (ThunkState {getThunk = t, getStep = step}) patchThunk haltThunk

    patchThunk :: ThunkState node x -> Thunk x -> IO (M.Step (Thunk x) node)
    patchThunk state t2 = do
      let ThunkState t1 prev = state
      if unsafeEqThunk t1 t2
        then pure $ M.Step (M.extract prev) state patchThunk haltThunk
        else do
          step <- M.step prev (runThunk t2)
          pure $ M.Step (M.extract step) (ThunkState {getStep = step, getThunk = t2}) patchThunk haltThunk

    haltThunk :: ThunkState node x -> IO ()
    haltThunk state = M.halt (getStep state)
