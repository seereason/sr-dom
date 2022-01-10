{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE RankNTypes #-}

module SeeReason.DOM.Monad where

import Control.Monad.Except
import Control.Monad.Trans
import Control.Exception
import System.IO

data DOMError = DOMError | DOMError_IO IOException deriving (Show)

newtype DOM a = DOM { unDOM :: ExceptT DOMError IO a } deriving (Functor, Applicative, Monad)

instance MonadIO DOM where
  liftIO = DOM . lift

runDOM :: DOM a -> IO (Either DOMError a)
runDOM = flip catch f . runExceptT . unDOM
  where f :: IOException -> IO (Either DOMError a)
        f e = return (Left (DOMError_IO e))

prop_runDOM = do
  runDOM (liftIO $ print "hello")
  runDOM (liftIO $ readFile "unknown.file")

  
