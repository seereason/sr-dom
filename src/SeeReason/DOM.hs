{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE RankNTypes #-}

module DOMT where

import Control.Monad.Except
import Control.Monad.Trans

data DOMError = DOMError | DOMError_IO IOError deriving (Show)

newtype DOMT a = DOMT { unDOMT :: ExceptT DOMError IO a } deriving (Functor, Applicative, Monad)

instance MonadIO DOMT where
  liftIO = DOMT . lift

runDOMT :: DOMT a -> IO (Either DOMError a)
runDOMT = runExceptT . unDOMT


test = runDOMT m0

m0 :: DOMT ()
m0 = liftIO $ print "hello"

  
