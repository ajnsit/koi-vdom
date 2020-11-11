{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Koi.VDom.DOM.Prop
  ( Prop (..),
    ElemRef (..),
    buildProp,
  )
where

import qualified Data.IORef as IORef
import qualified Data.Map as Map
import Koi.VDom.HostConfig (HostConfig)
import qualified Koi.VDom.HostConfig as HConf
import Koi.VDom.Machine as V
import Koi.VDom.Util as Util

-- | Attributes, properties, event handlers, and element lifecycles.
-- | Parameterized by the type of handlers outputs.
data Prop evt node a
  = Attribute String String
  | Handler String (evt -> Maybe a)
  | Ref (ElemRef node -> Maybe a)

instance Functor (Prop evt node) where
  fmap f (Handler ty g) = Handler ty (fmap f <$> g)
  fmap f (Ref g) = Ref (fmap f <$> g)
  fmap _ (Attribute k v) = Attribute k v

data ElemRef a
  = Created a
  | Removed a

instance Functor ElemRef where
  fmap f (Created a) = Created (f a)
  fmap f (Removed a) = Removed (f a)

data PState a b = PState
  { pStateEvents :: Object a,
    pStateProps :: Object b
  }

-- | A `Machine`` for applying attributes, properties, and event handlers.
-- | An emitter effect must be provided to respond to events. For example,
-- | to allow arbitrary effects in event handlers, one could use `id`.
buildProp ::
  forall node a.
  HostConfig node =>
  (a -> IO ()) ->
  node ->
  V.Machine [Prop (HConf.Event node) node a] ()
buildProp emit el = renderProp
  where
    renderProp ps1 = do
      events <- unsafeEmpty
      ps1' <- Util.strMapWithIxE ps1 propToStrKey (applyProp events)
      let state =
            PState
              { pStateEvents = events,
                pStateProps = ps1'
              }
      pure $ Step () state patchProp haltProp

    patchProp state ps2 = do
      events <- unsafeEmpty
      let PState {pStateEvents = prevEvents, pStateProps = ps1} = state
          onThese = diffProp prevEvents events
          onThis = removeProp prevEvents
          onThat = applyProp events
      props <- Util.diffWithKeyAndIxE ps1 ps2 propToStrKey onThese onThis onThat
      let nextState =
            PState
              { pStateEvents = events,
                pStateProps = props
              }
      pure $ Step () nextState patchProp haltProp

    haltProp state = do
      m1 <- IORef.readIORef (pStateProps state)
      case Map.lookup "ref" m1 of
        Just (Ref f) ->
          mbEmit (f (Removed el))
        _ -> pure ()

    mbEmit = \case
      Just a -> emit a
      _ -> pure ()

    applyProp events _ _ v =
      case v of
        Attribute attr val -> do
          HConf.setAttribute attr val el
          pure v
        Handler ty f -> do
          maybeHandler <- unsafeGetMaybe ty events
          case maybeHandler of
            Just handler -> do
              IORef.writeIORef (snd handler) f
              pure v
            _ -> do
              ref <- IORef.newIORef f
              -- listener <- DOM.eventListener \ev -> do
              let listener ev = do
                    f' <- IORef.readIORef ref
                    mbEmit (f' ev)
              pokeMutMap ty (listener, ref) events
              HConf.addEventListener ty listener el
              pure v
        Ref f -> do
          mbEmit (f (Created el))
          pure v

    diffProp prevEvents events _ _ v1 v2 =
      case (v1, v2) of
        (Attribute _ val1, Attribute attr2 val2) ->
          if val1 == val2
            then pure v2
            else do
              HConf.setAttribute attr2 val2 el
              pure v2
        -- (Property _ val1, Property prop2 val2) -> do
        --   eq <- Util.refEq val1 val2
        --   case (eq, prop2) of
        --     (True, _) ->
        --       pure v2
        --     (_, "value") -> do
        --       elVal <- unsafeGetProperty "value" el
        --       eq <- Util.refEq elVal val2
        --       if eq
        --         then pure v2
        --         else do
        --           setProperty prop2 val2 el
        --           pure v2
        --     (_, _) -> do
        --       setProperty prop2 val2 el
        --       pure v2
        (Handler _ _, Handler ty f) -> do
          handler <- unsafeGet ty prevEvents
          IORef.writeIORef (snd handler) f
          pokeMutMap ty handler events
          pure v2
        (_, _) ->
          pure v2

    removeProp prevEvents _ v =
      case v of
        Attribute attr _ ->
          HConf.removeAttribute attr el
        Handler ty _ -> do
          handler <- unsafeGet ty prevEvents
          HConf.removeEventListener ty (fst handler) el
        Ref _ ->
          pure ()

propToStrKey :: forall evt node i. Prop evt node i -> String
propToStrKey = \case
  Attribute attr _ -> "attr" <> ":" <> attr
  Attribute attr _ -> "attr/:" <> attr
  Handler ty _ -> "handler/" <> ty
  Ref _ -> "ref"
