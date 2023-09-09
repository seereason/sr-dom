{-# language JavaScriptFFI, OverloadedStrings #-}

module SeeReason.DOM.CallStack where

import Control.Monad.Trans
import GHC.Stack (CallStack, HasCallStack, callStack)
import GHCJS.DOM.Types (JSString, JSVal, ToJSString(..), FromJSString, fromJSString, toJSString, PToJSVal(..), pToJSVal, IsGObject)
import SeeReason.DOM.Types
import JavaScript.Web.Storage  


storeLocal :: (HasCallStack, MonadIO m, ToJSString s) => s -> s -> m ()
storeLocal key value = liftIO $ setItem (toJSString key) (toJSString value) localStorage

storeCallStack :: (HasCallStack, MonadIO m, ToJSString s) => s -> m ()
storeCallStack key = liftIO $ setItem (toJSString key) (toJSString $ show callStack) localStorage


documentContains :: (HasCallStack, MonadIO m) => JSVal -> m Bool
documentContains = liftIO . js_documentContains

typeof :: (HasCallStack, MonadIO m) => JSVal -> m String
typeof = fmap fromJSString . liftIO . js_typeof

foreign import javascript unsafe "$r = document.contains($1)"
        js_documentContains :: JSVal -> IO Bool

foreign import javascript unsafe "$r = typeof ($1)"
        js_typeof :: JSVal -> IO JSString



mySetAttribute :: (MonadIO m, ToJSString k, ToJSString v) => JSVal -> k -> v -> m ()
mySetAttribute e k v = liftIO $ js_mySetAttribute e (toJSString k) (toJSString v)

foreign import javascript unsafe "($1)[$2]=$3"
        js_mySetAttribute :: JSVal -> JSString -> JSString -> IO ()


className :: (MonadIO m, PToJSVal o, FromJSString s) => o -> m s
className = fmap fromJSString . liftIO . js_className . pToJSVal

foreign import javascript unsafe "$r = ($1).constructor.name"
        js_className :: JSVal -> IO JSString
