{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Koi.VDom.DOM
  ( VDomSpec (..),
    buildVDom,
    buildText,
    buildElem,
    buildKeyed,
    buildWidget,
    buildDecoration,
  )
where

import Koi.VDom.HostConfig (HostConfig, createElement, createTextNode, insertChildIx, parentNode, removeChild, setTextContent)
import Koi.VDom.Machine as Machine
import Koi.VDom.Types (ElemName (..), VDom (..), runGraft)
import Koi.VDom.Util as Util
import Prelude

type VDomMachine node a w = Machine (VDom a w) node

type VDomStep node a w = Step (VDom a w) node

-- type VDomInit node i a w = i -> IO (VDomStep node a w)

type VDomBuilder node i a w = VDomSpec node a w -> VDomMachine node a w -> i -> IO (VDomStep node a w)

type VDomBuilder4 node j k l a w = VDomSpec node a w -> VDomMachine node a w -> j -> k -> l -> IO (VDomStep node a w)

-- | Widget machines recursively reference the configured spec to potentially
-- | enable recursive trees of Widgets.
data VDomSpec node a w = VDomSpec
  { vdomSpecBuildWidget :: VDomSpec node a w -> Machine w node,
    vdomSpecBuildAttributes :: node -> Machine a ()
  }

-- | Starts an initial `VDom` machine by providing a `VDomSpec`.
-- |
-- | ```purescript
-- | main = do
-- |   machine1 <- buildVDom spec vdomTree1
-- |   machine2 <- Machine.step machine1 vdomTree2
-- |   machine3 <- Machine.step machine2 vdomTree3
-- |   ...
-- | ````
buildVDom :: forall a w node. HostConfig node => VDomSpec node a w -> VDomMachine node a w
buildVDom spec = build
  where
    build = \case
      Text s -> buildText spec build s
      Elem n a ch -> buildElem spec build n a ch
      Keyed n a ch -> buildKeyed spec build n a ch
      Widget w -> buildWidget spec build w
      Grafted g -> build (runGraft g)

data TextState node a w = TextState
  { textBuild :: VDomMachine node a w,
    textNode :: node,
    textValue :: String
  }

buildText :: forall a w node. HostConfig node => VDomBuilder node String a w
buildText _ textBuild s = do
  textNode <- createTextNode s
  let state = TextState {textBuild, textNode, textValue = s}
  pure $ Step textNode state patchText haltText

patchText :: forall a w node. HostConfig node => TextState node a w -> VDom a w -> IO (VDomStep node a w)
patchText state vdom = do
  let TextState {textBuild = build, textNode = node, textValue = value1} = state
  case vdom of
    Grafted g -> patchText state (runGraft g)
    Text value2
      | value1 == value2 ->
        pure $ Step node state patchText haltText
      | otherwise -> do
        let nextState = TextState {textBuild = build, textNode = node, textValue = value2}
        setTextContent value2 node
        pure $ Step node nextState patchText haltText
    _ -> do
      haltText state
      build vdom

haltText :: forall a w node. HostConfig node => TextState node a w -> IO ()
haltText (TextState {textNode = node}) = do
  parent <- parentNode node
  removeChild node parent

data ElemState node a w = ElemState
  { elemBuild :: VDomMachine node a w,
    elemNode :: node,
    elemAttrs :: Step a (),
    elemName :: ElemName,
    elemChildren :: [VDomStep node a w]
  }

buildElem :: forall a w node. HostConfig node => VDomBuilder4 node ElemName a [VDom a w] a w
buildElem (VDomSpec {vdomSpecBuildAttributes}) build name1 as1 ch1 = do
  node <- createElement name1
  let onChild ix child = do
        res <- build child
        insertChildIx ix (extract res) node
        pure res
  children <- Util.forE ch1 onChild
  attrs <- vdomSpecBuildAttributes node as1
  let state =
        ElemState
          { elemBuild = build,
            elemNode = node,
            elemAttrs = attrs,
            elemName = name1,
            elemChildren = children
          }
  pure $ Step node state patchElem haltElem

patchElem :: forall a w node. HostConfig node => ElemState node a w -> VDom a w -> IO (VDomStep node a w)
patchElem = patchElemGo
  where
    haltElemGo = haltElem
    patchElemGo state vdom = do
      let ElemState {elemBuild = build, elemNode = node, elemAttrs = attrs, elemName = name1, elemChildren = ch1} = state
      case vdom of
        Grafted g -> patchElemGo state (runGraft g)
        Elem name2 as2 ch2 | eqElemSpec name1 name2 -> do
          case (length ch1, length ch2) of
            (0, 0) -> do
              attrs2 <- step attrs as2
              let nextState =
                    ElemState
                      { elemBuild = build,
                        elemNode = node,
                        elemAttrs = attrs2,
                        elemName = name2,
                        elemChildren = ch1
                      }
              pure $ Step node nextState patchElemGo haltElemGo
            _ -> do
              let onThese ix s v = do
                    res <- step s v
                    insertChildIx ix (extract res) node
                    pure res
                  onThis _ s = halt s
                  onThat ix v = do
                    res <- build v
                    insertChildIx ix (extract res) node
                    pure res
              children2 <- Util.diffWithIxE ch1 ch2 onThese onThis onThat
              attrs2 <- step attrs as2
              let nextState =
                    ElemState
                      { elemBuild = build,
                        elemNode = node,
                        elemAttrs = attrs2,
                        elemName = name2,
                        elemChildren = children2
                      }
              pure $ Step node nextState patchElemGo haltElemGo
        _ -> do
          haltElemGo state
          build vdom

haltElem :: forall a w node. HostConfig node => ElemState node a w -> IO ()
haltElem (ElemState {elemNode = node, elemAttrs = attrs, elemChildren = children}) = do
  parent <- parentNode node
  removeChild node parent
  Util.forEachE children halt
  halt attrs

data KeyedState node a w = KeyedState
  { keyedBuild :: VDomMachine node a w,
    keyedNode :: node,
    keyedAttrs :: Step a (),
    keyedName :: ElemName,
    keyedChildren :: Util.Object (VDomStep node a w),
    keyedLength :: Int
  }

buildKeyed :: forall a w node. HostConfig node => VDomBuilder4 node ElemName a [(String, VDom a w)] a w
buildKeyed (VDomSpec {vdomSpecBuildAttributes}) build name1 as1 ch1 = do
  node <- createElement name1
  let onChild _ ix (_, vdom) = do
        res <- build vdom
        insertChildIx ix (extract res) node
        pure res
  children <- Util.strMapWithIxE ch1 fst onChild
  attrs <- vdomSpecBuildAttributes node as1
  let state =
        KeyedState
          { keyedBuild = build,
            keyedNode = node,
            keyedAttrs = attrs,
            keyedName = name1,
            keyedChildren = children,
            keyedLength = length ch1
          }
  pure $ Step node state patchKeyed haltKeyed

patchKeyed :: forall a w node. HostConfig node => KeyedState node a w -> VDom a w -> IO (VDomStep node a w)
patchKeyed state vdom = do
  let KeyedState {keyedBuild, keyedNode, keyedAttrs, keyedName = name1, keyedChildren = ch1, keyedLength = len1} = state
  case vdom of
    Grafted g -> patchKeyed state (runGraft g)
    Keyed name2 as2 ch2 | eqElemSpec name1 name2 ->
      case (len1, length ch2) of
        (0, 0) -> do
          attrs2 <- Machine.step keyedAttrs as2
          let nextState =
                KeyedState
                  { keyedBuild,
                    keyedNode,
                    keyedAttrs = attrs2,
                    keyedName = name2,
                    keyedChildren = ch1,
                    keyedLength = 0
                  }
          pure $ Step keyedNode nextState patchKeyed haltKeyed
        (_, len2) -> do
          let onThese _ ix' s (_, v) = do
                res <- step s v
                -- AJ: TODO: This should check if the parents are the same and then not insert the new child
                -- AJ: TODO: This is harder to detect and optimise on the backend with lesser information
                insertChildIx ix' (extract res) keyedNode
                pure res
              onThis _ s = halt s
              onThat _ ix (_, v) = do
                res <- keyedBuild v
                insertChildIx ix (extract res) keyedNode
                pure res
          children2 <- Util.diffWithKeyAndIxE ch1 ch2 fst onThese onThis onThat
          attrs2 <- step keyedAttrs as2
          let nextState =
                KeyedState
                  { keyedBuild,
                    keyedNode,
                    keyedAttrs = attrs2,
                    keyedName = name2,
                    keyedChildren = children2,
                    keyedLength = len2
                  }
          pure $ Step keyedNode nextState patchKeyed haltKeyed
    _ -> do
      haltKeyed state
      keyedBuild vdom

haltKeyed :: forall a w node. HostConfig node => KeyedState node a w -> IO ()
haltKeyed (KeyedState {keyedNode = node, keyedAttrs = attrs, keyedChildren = children}) = do
  parent <- parentNode node
  removeChild node parent
  Util.forInE children (\_ s -> halt s)
  halt attrs

data WidgetState node a w = WidgetState
  { widgetBuild :: VDomMachine node a w,
    widgetWidget :: Step w node
  }

buildWidget :: forall a w node. VDomBuilder node w a w
buildWidget spec@(VDomSpec {vdomSpecBuildWidget}) build w = do
  res@(Step n _ _ _) <- vdomSpecBuildWidget spec w
  pure $ Step n (WidgetState {widgetBuild = build, widgetWidget = res}) patchWidget haltWidget

patchWidget :: forall a w node. WidgetState node a w -> VDom a w -> IO (VDomStep node a w)
patchWidget state vdom = do
  let WidgetState {widgetBuild = build, widgetWidget = widget} = state
  case vdom of
    Grafted g -> patchWidget state (runGraft g)
    Widget w -> do
      res@(Step n _ _ _) <- step widget w
      pure $ Step n (WidgetState {widgetBuild = build, widgetWidget = res}) patchWidget haltWidget
    _ -> do
      haltWidget state
      build vdom

haltWidget :: forall a w node. WidgetState node a w -> IO ()
haltWidget (WidgetState {widgetWidget = widget}) = halt widget

eqElemSpec :: ElemName -> ElemName -> Bool
eqElemSpec (ElemName name1) (ElemName name2) = name1 == name2

-- A Simple decorated-vdom machine that just delegates everything to the underlying vdom
buildDecoration ::
  forall decor a w node.
  HostConfig node =>
  (decor (VDom a w) -> VDom a w) ->
  VDomSpec node a w ->
  Machine (decor (VDom a w)) node
buildDecoration runDecoration = renderDecoration
  where
    renderDecoration :: VDomSpec node a w -> Machine (decor (VDom a w)) node
    renderDecoration spec t = do
      vdom <- buildVDom spec (runDecoration t)
      pure $ Step (extract vdom) vdom patchDecoration halt

    patchDecoration :: Step (VDom a w) node -> decor (VDom a w) -> IO (Step (decor (VDom a w)) node)
    patchDecoration vdomPrev t = do
      vdom <- step vdomPrev (runDecoration t)
      pure $ Step (extract vdom) vdom patchDecoration halt
