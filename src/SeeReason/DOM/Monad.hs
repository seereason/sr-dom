{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE RankNTypes #-}

module SeeReason.DOM.Monad where

import Control.Monad.Except
import Control.Monad.Trans
import Control.Exception
import SeeReason.DOM.Types
import System.IO

runDOM :: DOM a -> IO (Either DH_Error a)
runDOM = flip catch f . runExceptT . unDOM
  where f :: IOException -> IO (Either DH_Error a)
        f e = return (Left (DH_IOException e))

prop_runDOM = do
  runDOM (liftIO $ print "hello")
  runDOM (liftIO $ readFile "unknown.file")

  
