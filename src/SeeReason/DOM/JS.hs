{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, ConstraintKinds, ExtendedDefaultRules, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module SeeReason.DOM.JS
  ( createElement
  , createTextNode
  , body
  , document
  , appendChild
  ) where

import Control.Monad.Trans
import SeeReason.DOM.Monad

import GHCJS.DOM.Types as DOM (Document, Element) -- (Element, Event, IsEvent)
import GHCJS.DOM.Event (Event, IsEvent, toEvent)
import GHCJS.Foreign.Callback (OnBlocked(..), Callback, asyncCallback, asyncCallback1, releaseCallback, syncCallback1)
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))

import GHCJS.Types (JSVal)
import JavaScript.Object (Object, getProp)
import JavaScript.Cast

import qualified GHCJS.DOM.History as DOM (pushState)
import qualified GHCJS.DOM.Window as DOM (getHistory)
import qualified GHCJS.DOM as DOM (currentWindowUnchecked)
import GHCJS.DOM.Types (FromJSString, ToJSString, FromJSVal(..), ToJSVal(..), Window, toJSString, fromJSString, fromMaybeJSString, castTo)


foreign import javascript unsafe "$r = ($1).createElement($2)"
  js_createElement :: Document -> JSString -> IO DOM.Element


createElement :: ToJSString tag => Document -> tag -> DOM Element
createElement doc tag = DOM $ do
  liftIO $ js_createElement doc (toJSString tag)

foreign import javascript unsafe "$r = ($1).createTextNode($2)"
  js_createTextNode :: Document -> JSString -> IO DOM.Element

createTextNode :: ToJSString t => Document -> t -> DOM Element
createTextNode doc t = DOM $ do
  liftIO $ js_createTextNode doc (toJSString t)

foreign import javascript unsafe "$r = document"
  js_document :: IO Document

document :: DOM Document
document = DOM $ do
  liftIO $ js_document

foreign import javascript unsafe "$r = $1.body"
  js_body :: Document -> IO Element

body :: Document -> DOM Element
body = DOM . liftIO . js_body
  

foreign import javascript unsafe "$1.appendChild($2)"
   js_appendChild :: Element -> Element -> IO ()

appendChild :: Element -> Element -> DOM ()
appendChild parent child = DOM . liftIO $ do
  js_appendChild parent child


