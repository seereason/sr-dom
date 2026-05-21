{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, ConstraintKinds, ExtendedDefaultRules, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module SeeReason.DOM.JS
  ( createElement
  , createTextNode
  , getElementById
  , setAttribute
  , removeAttribute
  , setProperty
  , deleteProperty
  , body
  , document
  , window
  , appendChild
  , eventTargetAddEventListener
  , eventTargetAddEventListenerOpt
  , waitReady
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans
import Control.Monad.Except
import SeeReason.DOM.Types (DOM(..), DH_Error(..))

import GHCJS.DOM.Types as GD (Document, Element, EventTarget, IsEventTarget, toEventTarget, ToJSString)
import GHCJS.DOM.Event as GD (Event(..))
import GHC.JS.Foreign.Callback (OnBlocked(..), Callback, releaseCallback, syncCallback1)
import GHCJS.Marshal.Pure (PToJSVal(..))
import GHCJS.Types (JSVal, JSString)
import GHCJS.DOM.Types (Window, toJSString)

-- | invokes document.getElementById(ident)
getElementById :: ToJSString ident => Document -> ident -> DOM Element
getElementById doc ident = DOM $ do
  liftIO $ js_getElementById doc (toJSString ident)

#if defined(__GHCJS__)
foreign import javascript unsafe "$r = ($1).getElementById($2)"
  js_getElementById :: Document -> JSString -> IO Element
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return (a1).getElementById(a2); })"
  js_getElementById :: Document -> JSString -> IO Element
#endif

-- | invokes document.createElement(tag)
createElement :: ToJSString tag => Document -> tag -> DOM Element
createElement doc tag = DOM $ do
  liftIO $ js_createElement doc (toJSString tag)

#if defined(__GHCJS__)
foreign import javascript unsafe "$r = ($1).createElement($2)"
  js_createElement :: Document -> JSString -> IO Element
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return (a1).createElement(a2); })"
  js_createElement :: Document -> JSString -> IO Element
#endif

-- | invokes document.createTextNode(text)
createTextNode :: ToJSString t => Document -> t -> DOM Element
createTextNode doc t = DOM $ do
  liftIO $ js_createTextNode doc (toJSString t)

#if defined(__GHCJS__)
foreign import javascript unsafe "$r = ($1).createTextNode($2)"
  js_createTextNode :: Document -> JSString -> IO Element
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return (a1).createTextNode(a2); })"
  js_createTextNode :: Document -> JSString -> IO Element
#endif

-- | invokes document.setAttribute(name, value)
setAttribute :: ToJSString t => Element -> t -> t -> DOM Element
setAttribute e name value = DOM $ do
  liftIO $ js_setAttribute e (toJSString name) (toJSString value)
  return e
#if defined(__GHCJS__)
foreign import javascript unsafe "($1).setAttribute($2,$3)"
  js_setAttribute :: Element -> JSString -> JSString -> IO ()
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => { (a1).setAttribute(a2,a3); })"
  js_setAttribute :: Element -> JSString -> JSString -> IO ()
#endif

-- | invokes document.removeAttribute(name)
removeAttribute :: ToJSString t => Element -> t -> DOM Element
removeAttribute e name = DOM $ do
  liftIO $ js_removeAttribute e (toJSString name)
  return e
#if defined(__GHCJS__)
foreign import javascript unsafe "($1).removeAttribute($2)"
  js_removeAttribute :: Element -> JSString -> IO ()
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { (a1).removeAttribute(a2); })"
  js_removeAttribute :: Element -> JSString -> IO ()
#endif

-- | property assignment specialized to Elements.
-- This probably is unnecessary, but it is convenient to have a
-- parallel to setAttribute.
setProperty :: (ToJSString t, PToJSVal v) => Element -> t -> v -> DOM Element
setProperty e name value = DOM $ do
  liftIO $ js_setProperty e (toJSString name) (pToJSVal value)
  return e
#if defined(__GHCJS__)
foreign import javascript unsafe "($1)[$2]=($3)"
  js_setProperty :: Element -> JSString -> JSVal -> IO ()
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1,a2,a3) => { (a1)[a2]=(a3); })"
  js_setProperty :: Element -> JSString -> JSVal -> IO ()
#endif

-- | invokes delete e[name]
deleteProperty :: (ToJSString s) => Element -> s -> DOM Element
deleteProperty e name = DOM $ do
  liftIO $ js_deleteProperty e (toJSString name)
  return e
#if defined(__GHCJS__)
foreign import javascript unsafe "delete ($1)[$2]"
  js_deleteProperty :: Element -> JSString -> IO ()
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { delete (a1)[a2]; })"
  js_deleteProperty :: Element -> JSString -> IO ()
#endif

-- | invokes document
window :: DOM Window
window = DOM $ do
  liftIO $ js_window

#if defined(__GHCJS__)
foreign import javascript unsafe "$r = window"
  js_window :: IO Window
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "(() => { return window; })"
  js_window :: IO Window
#endif

-- | invokes document
document :: DOM Document
document = DOM $ do
  liftIO $ js_document

#if defined(__GHCJS__)
foreign import javascript unsafe "$r = document"
  js_document :: IO Document
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "(() => { return document; })"
  js_document :: IO Document
#endif

#if 0
(.:) :: (IsGObject o, PToJSVal o, ToJSString p, PFromJSVal r) => o -> p -> DOM r
(.:) = getProperty

getProperty :: (IsGObject o, PToJSVal o, ToJSString p, PFromJSVal a) => o -> p -> DOM a
getProperty obj prop = DOM $ do
  v <- liftIO $ js_getProp (pToJSVal obj) (toJSString prop)
  let v' = (nullableToMaybe . Nullable) v
  case v' of
    Nothing -> throwError (DH_PropertyNotFound (asText prop))
    Just ja -> return (pFromJSVal ja)

#if defined(__GHCJS__)
foreign import javascript unsafe "$r = $1[$2]"
  js_getProp :: JSVal -> JSString -> IO JSVal
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { return a1[a2]; })"
  js_getProp :: JSVal -> JSString -> IO JSVal
#endif
#endif


-- | invokes document.body
body :: Document -> DOM Element
body = DOM . liftIO . js_body

#if defined(__GHCJS__)
foreign import javascript unsafe "$r = $1.body"
  js_body :: Document -> IO Element
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1.body; })"
  js_body :: Document -> IO Element
#endif

-- | invokes parent.appendChild(child)
appendChild :: Element -> Element -> DOM ()
appendChild parent child = DOM . liftIO $ do
  js_appendChild parent child

#if defined(__GHCJS__)
foreign import javascript unsafe "$1.appendChild($2)"
   js_appendChild :: Element -> Element -> IO ()
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1,a2) => { a1.appendChild(a2); })"
   js_appendChild :: Element -> Element -> IO ()
#endif

-- TODO make this fail after some amount of time.
waitReady :: Document -> DOM ()
waitReady doc = waitReady' (100,3) doc

-- | waitReady' is the same as waitReady, but accepts a pair integers to configure (delayTime, retryCount)
waitReady' :: (Int,Int) -> Document -> DOM ()
waitReady' (_, retryCount) _ | retryCount <= 0 = throwError DH_DocumentNotReady
waitReady' (delayTime, retryCount) doc = do
  s <- liftIO $ js_readyState doc
  case s of
    "complete" -> pure ()
    _ -> liftIO (threadDelay delayTime) >> waitReady' (delayTime, pred retryCount) doc

#if defined(__GHCJS__)
foreign import javascript unsafe "$r = $1[\"readyState\"]"
  js_readyState :: Document -> IO JSString
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe "((a1) => { return a1[\"readyState\"]; })"
  js_readyState :: Document -> IO JSString
#endif

#if __GHCJS__
foreign import javascript unsafe
        "$1.addEventListener($2, $3, $4)"
        addEventListener ::
        EventTarget -> JSString -> Callback a -> Bool -> IO ()

foreign import javascript unsafe
        "$1.removeEventListener($2, $3, $4)"
        removeEventListener ::
        EventTarget -> JSString -> Callback a -> Bool -> IO Bool

foreign import javascript unsafe
        "$1.addEventListener($2, $3, { capture: $4, once: $5, passive: $6})"
        addEventListenerOpt ::
        EventTarget -> JSString -> Callback a -> Bool -> Bool -> Bool -> IO ()

foreign import javascript unsafe
        "$1.removeEventListener($2, $3, { capture: $4, once: $5, passive: $6})"
        removeEventListenerOpt ::
        EventTarget -> JSString -> Callback a -> Bool -> Bool -> Bool -> IO Bool
#elif defined(javascript_HOST_ARCH)
foreign import javascript unsafe
        "((self,evtype,cb,useCapture) => { return self[\"addEventListener\"](evtype, cb, useCapture); })"
        addEventListener ::
        EventTarget -> JSString -> Callback a -> Bool -> IO ()

foreign import javascript unsafe
        "((self,evtype,cb,useCapture) => { return self[\"removeEventListener\"](evtype,cb,useCapture); })"
        removeEventListener ::
        EventTarget -> JSString -> Callback a -> Bool -> IO Bool

foreign import javascript unsafe
        "((self,evtype,cb,capture,once,passive) => { return self[\"addEventListener\"](evtype, cb, { capture: capture, once: once, passive: passive}); })"
        addEventListenerOpt ::
        EventTarget -> JSString -> Callback a -> Bool -> Bool -> Bool -> IO ()

foreign import javascript unsafe
        "((self,evtype,cb,capture,once,passive) => { return self[\"removeEventListener\"](evtype, cb, { capture: capture, once: once, passive: passive}); })"
        removeEventListenerOpt ::
        EventTarget -> JSString -> Callback a -> Bool -> Bool -> Bool -> IO Bool
#endif


-- TODO support all these options correctly, with documentation and tests

eventTargetAddEventListener :: (ToJSString s, IsEventTarget e) =>  e -> s -> Bool
                               -> (e -> GD.Event -> IO ()) -> DOM (DOM ())
eventTargetAddEventListener obj eventName bubble user =
  wrap (eventTargetAddEventListener' obj (toJSString eventName) bubble user)
  where wrap = dom . fmap dom
        dom = DOM . liftIO

eventTargetAddEventListener' :: IsEventTarget a =>  a -> JSString -> Bool
                               -> (a -> GD.Event -> IO ()) -> IO (IO ())
eventTargetAddEventListener' obj eventName bubble user = do
    -- putStrLn "Alderon.MicroDOM.eventTargetAddEventListener"
    callback <- syncCallback1 ContinueAsync $ \e -> user obj (Event e)
    addEventListener
        (toEventTarget obj)
        eventName
        callback
        bubble
    return $ do
        _ <- removeEventListener
             (toEventTarget obj)
             eventName
             callback
             bubble
        releaseCallback callback

eventTargetAddEventListenerOpt :: IsEventTarget a =>  a -> JSString -> Bool -> Bool -> Bool
                               -> (a -> GD.Event -> IO ()) -> DOM (DOM ())
eventTargetAddEventListenerOpt obj eventName capture once passive user =
  wrap (eventTargetAddEventListenerOpt' obj eventName capture once passive user)
  where wrap = dom . fmap dom
        dom = DOM . liftIO

eventTargetAddEventListenerOpt' :: IsEventTarget a =>  a -> JSString -> Bool -> Bool -> Bool
                               -> (a -> GD.Event -> IO ()) -> IO (IO ())
eventTargetAddEventListenerOpt' obj eventName capture once passive user = do
    callback <- syncCallback1 ContinueAsync $ \e -> user obj (Event e)
    addEventListenerOpt
        (toEventTarget obj)
        eventName
        callback
        capture once passive
    return $ do
        _ <- removeEventListenerOpt
             (toEventTarget obj)
             eventName
             callback
             capture once passive
        releaseCallback callback
