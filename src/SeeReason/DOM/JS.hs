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
  ) where

import Control.Monad.Trans
import Control.Monad.Except
import SeeReason.DOM.Types (DOM(..), DH_Error(..), asText)

import GHCJS.DOM.Types as GD (Document, Element, EventTarget, IsEventTarget, toEventTarget, IsGObject, FromJSString, ToJSString)
import qualified GHCJS.DOM.MouseEvent as GD (MouseEvent(..), getButton, getClientX, getClientY, getScreenX, getScreenY)
import GHCJS.DOM.Event as GD (Event(..), IsEvent, toEvent)
import GHCJS.Foreign.Callback (OnBlocked(..), Callback, asyncCallback, asyncCallback1, releaseCallback, syncCallback1)
import GHCJS.Foreign
import GHCJS.Marshal (ToJSVal)
import GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))
import GHCJS.Nullable (Nullable(..), nullableToMaybe)
import GHCJS.Types (JSVal, JSString)
import JavaScript.Object (Object, getProp)
import JavaScript.Cast

import qualified GHCJS.DOM.History as DOM (pushState)
import qualified GHCJS.DOM.Window as DOM (getHistory)
import qualified GHCJS.DOM as DOM (currentWindowUnchecked)
import GHCJS.DOM.Types (FromJSString, ToJSString, FromJSVal(..), ToJSVal(..), Window, toJSString, fromJSString, fromMaybeJSString, castTo)

-- | invokes document.getElementById(ident)
getElementById :: ToJSString ident => Document -> ident -> DOM Element
getElementById doc ident = DOM $ do
  liftIO $ js_getElementById doc (toJSString ident)

foreign import javascript unsafe "$r = ($1).getElementById($2)"
  js_getElementById :: Document -> JSString -> IO Element

-- | invokes document.createElement(tag)
createElement :: ToJSString tag => Document -> tag -> DOM Element
createElement doc tag = DOM $ do
  liftIO $ js_createElement doc (toJSString tag)
foreign import javascript unsafe "$r = ($1).createElement($2)"
  js_createElement :: Document -> JSString -> IO Element

-- | invokes document.createTextNode(text)
createTextNode :: ToJSString t => Document -> t -> DOM Element
createTextNode doc t = DOM $ do
  liftIO $ js_createTextNode doc (toJSString t)

foreign import javascript unsafe "$r = ($1).createTextNode($2)"
  js_createTextNode :: Document -> JSString -> IO Element

-- | invokes document.setAttribute(name, value)
setAttribute :: ToJSString t => Element -> t -> t -> DOM Element
setAttribute e name value = DOM $ do
  liftIO $ js_setAttribute e (toJSString name) (toJSString value)
  return e
foreign import javascript unsafe "($1).setAttribute($2,$3)"
  js_setAttribute :: Element -> JSString -> JSString -> IO ()

-- | invokes document.removeAttribute(name)
removeAttribute :: ToJSString t => Element -> t -> DOM Element
removeAttribute e name = DOM $ do
  liftIO $ js_removeAttribute e (toJSString name)
  return e
foreign import javascript unsafe "($1).removeAttribute($2)"
  js_removeAttribute :: Element -> JSString -> IO ()

-- | property assignment specialized to Elements.
-- This probably is unnecessary, but it is convenient to have a
-- parallel to setAttribute.
setProperty :: (ToJSString t, PToJSVal v) => Element -> t -> v -> DOM Element
setProperty e name value = DOM $ do
  liftIO $ js_setProperty e (toJSString name) (pToJSVal value)
  return e
foreign import javascript unsafe "($1)[$2]=($3)"
  js_setProperty :: Element -> JSString -> JSVal -> IO ()

-- | invokes delete e[name]
deleteProperty :: (ToJSString s) => Element -> s -> DOM Element
deleteProperty e name = DOM $ do
  liftIO $ js_deleteProperty e (toJSString name)
  return e
foreign import javascript unsafe "delete ($1)[$2]"
  js_deleteProperty :: Element -> JSString -> IO ()

-- | invokes document
window :: DOM Window
window = DOM $ do
  liftIO $ js_window

foreign import javascript unsafe "$r = window"
  js_window :: IO Window

-- | invokes document
document :: DOM Document
document = DOM $ do
  liftIO $ js_document

foreign import javascript unsafe "$r = document"
  js_document :: IO Document

-- | invokes document
document' :: DOM Document
document' = do
  w <- window
  w .: "document"

(.:) :: (IsGObject o, PToJSVal o, ToJSString p, PFromJSVal r) => o -> p -> DOM r
(.:) = getProperty

getProperty :: (IsGObject o, PToJSVal o, ToJSString p, PFromJSVal a) => o -> p -> DOM a
getProperty obj prop = DOM $ do
  v <- liftIO $ js_getProp (pToJSVal obj) (toJSString prop)
  let v' = (nullableToMaybe . Nullable) v
  case v' of
    Nothing -> throwError (DH_PropertyNotFound (asText prop))
    Just ja -> return (pFromJSVal ja)

foreign import javascript unsafe "$r = $1[$2]"
  js_getProp :: JSVal -> JSString -> IO JSVal



-- | invokes document.body
body :: Document -> DOM Element
body = DOM . liftIO . js_body

foreign import javascript unsafe "$r = $1.body"
  js_body :: Document -> IO Element

-- | invokes parent.appendChild(child)
appendChild :: Element -> Element -> DOM ()
appendChild parent child = DOM . liftIO $ do
  js_appendChild parent child
  
foreign import javascript unsafe "$1.appendChild($2)"
   js_appendChild :: Element -> Element -> IO ()


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
        removeEventListener
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
        removeEventListenerOpt
            (toEventTarget obj)
            eventName
            callback
            capture once passive
        releaseCallback callback
