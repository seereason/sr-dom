{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ConstraintKinds, ExtendedDefaultRules, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses#-}


module SeeReason.DOM.Types where

import Control.Exception
import Control.Monad.Except
import Control.Monad (void, when)

import Control.Monad.Trans (MonadIO(..))
import Data.Aeson (ToJSON(toJSON))
import Data.Proxy
import Data.String
import Data.Text as T (Text, concat, pack)
import qualified GHCJS.DOM.Types as DOM -- (Element, Event, IsEvent)
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
import GHCJS.Types (JSString, jsval, JSException(..))
import GHCJS.Nullable (Nullable(..), nullableToMaybe)
import System.IO

data DH_Error =
  DH_Error |
  DH_PropertyNotFound Text |
  DH_IOException IOException deriving (Show)

asText :: ToJSString a => a -> Text
asText = fromJSString . toJSString

newtype DOM a = DOM { unDOM :: ExceptT DH_Error IO a } deriving (Functor, Applicative, Monad)

instance MonadIO DOM where
  liftIO = DOM . lift

instance MonadError DH_Error DOM where
  throwError e = DOM (throwError e)
  catchError m c = DOM (catchError (unDOM m) (\e -> unDOM (c e)))

#if 0
class JSArgs a where
    applyFunction :: JSVal b -> JSString -> a -> IO (JSVal c)

instance JSArgs () where
    applyFunction obj fun () = do
        res <- apply0 obj fun
        fromJSValue res

instance (JSValue a, JSValue b) => JSArgs (a, b) where
    applyFunction obj fun (a, b) = do
        arg1 <- toJSValue a
        arg2 <- toJSValue b
        res <- apply2 obj fun arg1 arg2
        fromJSValue res

instance (JSValue a, JSValue b, JSValue c) => JSArgs (a, b, c) where
    applyFunction obj fun (a, b, c) = do
        arg1 <- toJSValue a
        arg2 <- toJSValue b
        arg3 <- toJSValue c
        res <- apply3 obj fun arg1 arg2 arg3
        fromJSValue res


foreign import javascript unsafe "$r = $1[$2]();"
    apply0 :: JSVal -> JSString -> IO (JSRef b)

foreign import javascript unsafe "$r = $1[$2]($3);"
    apply1 :: JSRef a -> JSString -> JSRef b -> IO (JSRef c)

foreign import javascript unsafe "$r = $1[$2]($3, $4);"
    apply2 :: JSRef a -> JSString -> JSRef b -> JSRef c -> IO (JSRef d)

foreign import javascript unsafe "$r = $1[$2]($3, $4, $5);"
    apply3 :: JSRef a -> JSString
           -> JSRef b -> JSRef c -> JSRef d -> IO (JSRef e)
#endif
