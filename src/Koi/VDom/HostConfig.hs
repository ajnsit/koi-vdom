{-# LANGUAGE TypeFamilies #-}

module Koi.VDom.HostConfig where

import Koi.VDom.Types (ElemName, Namespace)

type RawEventListener node = Event node -> IO ()

-- TODO: Make EventListener an opaque type
type EventListener node = Event node -> IO ()

class HostConfig node where
  type Event node
  createTextNode :: String -> IO node
  setTextContent :: String -> node -> IO ()
  createElement :: ElemName -> IO node
  insertChildIx :: Int -> node -> node -> IO ()
  removeChild :: node -> node -> IO ()
  parentNode :: node -> IO node
  setAttribute :: String -> String -> node -> IO ()
  removeAttribute :: String -> node -> IO ()
  hasAttribute :: String -> node -> IO Bool
  addEventListener :: String -> RawEventListener node -> node -> IO (EventListener node)
  removeEventListener :: String -> EventListener node -> node -> IO ()
