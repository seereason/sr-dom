{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, ConstraintKinds, ExtendedDefaultRules, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module SeeReason.DOM.Call
  ( call, apply
  , JSArgs(..)
  ) where

import Control.Monad.Trans
import SeeReason.DOM.Types (DOM(..))

import GHCJS.DOM.Types as GD (Document, Element, EventTarget, IsEventTarget, toEventTarget, IsGObject, FromJSString, ToJSString)
import qualified GHCJS.DOM.MouseEvent as GD (MouseEvent(..), getButton, getClientX, getClientY, getScreenX, getScreenY)
import GHCJS.DOM.Event as GD (Event(..), IsEvent, toEvent)
import GHCJS.Foreign.Callback (OnBlocked(..), Callback, asyncCallback, asyncCallback1, releaseCallback, syncCallback1)
import GHCJS.Foreign
import GHCJS.Marshal (ToJSVal)
import GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))
import GHCJS.Types (JSVal, JSString)
import JavaScript.Object (Object, getProp)
import JavaScript.Cast

import qualified GHCJS.DOM.History as DOM (pushState)
import qualified GHCJS.DOM.Window as DOM (getHistory)
import qualified GHCJS.DOM as DOM (currentWindowUnchecked)
import GHCJS.DOM.Types (FromJSString, ToJSString, FromJSVal(..), ToJSVal(..), Window, toJSString, fromJSString, fromMaybeJSString, castTo)


-- | Apply a function to an argument.
call :: (IsGObject o, ToJSString fun, JSArgs a, PFromJSVal r) => o -> fun -> a -> DOM r
call obj fun b = DOM $ do
    let arg = pToJSVal b
    res <- liftIO $ apply1 obj (toJSString fun) arg
    return $ pFromJSVal res

-- | Apply a function to multiple arguments.
apply :: (IsGObject o, ToJSString fun, MonadIO m, JSArgs a, PFromJSVal r) => o -> fun -> a -> m r
apply obj fun b = liftIO $ do
    res <- applyFunction obj (toJSString fun) b
    return $ pFromJSVal res

class PToJSVal a => JSArgs a where
  applyFunction :: (IsGObject obj, ToJSString f, PFromJSVal r) => obj -> f -> a -> IO r

instance PToJSVal () where
  pToJSVal () = jsNull

--class JSArgs a where
--  applyFunction :: obj -> f -> a -> IO b

instance JSArgs () where
    applyFunction obj fun () = do
        res <- apply0 obj (toJSString fun)
        return $ pFromJSVal res

instance (PToJSVal a, PToJSVal b) => JSArgs (a, b) where
    applyFunction obj fun (a, b) = do
        let arg1 = pToJSValue a
            arg2 = pToJSValue b
        res <- apply2 obj fun arg1 arg2
        return (pFromJSVal res)

instance (PToJSVal a, PToJSVal b, PToJSVal c) => JSArgs (a, b, c) where
    applyFunction obj fun (a, b, c) = do
        let arg1 = pToJSValue a
            arg2 = pToJSValue b
            arg3 = pToJSValue c
        res <- apply3 obj fun arg1 arg2 arg3
        return (pFromJSVal res)


foreign import javascript unsafe "$r = $1[$2]();"
    apply0 :: IsGObject o => o -> JSString -> IO JSVal

foreign import javascript unsafe "$r = $1[$2]($3);"
    apply1 :: IsGObject o => o -> JSString -> JSVal -> IO JSVal

foreign import javascript unsafe "$r = $1[$2]($3, $4);"
    apply2 :: IsGObject o => o -> JSString -> JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe "$r = $1[$2]($3, $4, $5);"
    apply3 :: IsGObject o => o -> JSString -> JSVal -> JSVal -> JSVal -> IO JSVal

