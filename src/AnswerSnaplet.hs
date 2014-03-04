{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module AnswerSnaplet where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import qualified Data.ConfigFile as Config
import Control.Monad.Error
import System.Process
import System.Directory
import GHC.IO.Handle
import System.IO
import Data.IORef
import Control.Monad.State


----------------------------------------------------------------------------
moveInAnswerer :: FilePath -> FilePath -> FilePath -> IO ()
moveInAnswerer logLoc oldLoc newLoc = do
  exAns <- doesFileExist oldLoc
  if not exAns
    then do
      putStrLn "No answerer found: using empty answerer"
      writeFile logLoc ("No answerer found: moving emptyAnswerer to " ++ newLoc ++ ".")
      writeFile logLoc "\n"
      copyFile "snaplets/answerer/EmptyAnswerer" newLoc
    else do
      putStrLn "Moving in new answerer"
      copyFile oldLoc newLoc

spawnProcess fp = do
  (Just inh,Just outh,_,prc) <-
    createProcess (proc fp []){
      std_in = CreatePipe,
      std_out = CreatePipe}
  return (inh,outh,prc)

------------------------------------------------------------------------------
data Answerer = Answerer
    { _answererOrigLocation :: FilePath,
      _answererTmpLocation :: FilePath,
      _logLocation :: FilePath,
      _doUpdate :: IORef Bool
    }


initAnswerer fp =
  runErrorT $ do
    cp <- join $ liftIO (Config.readfile Config.emptyCP fp)
    logLoc <- Config.get cp "DEFAULT" "logfile"
    tmpLoc <- Config.get cp "DEFAULT" "tmploc"
    answererLoc <- Config.get cp "DEFAULT" "executableloc"
    liftIO $ moveInAnswerer logLoc answererLoc tmpLoc
    up <- liftIO $ newIORef False
    return (Answerer answererLoc tmpLoc logLoc up)

makeLenses ''Answerer
------------------------------------------------------------------------------

type AnswerHandler = Handler Answerer Answerer


checkForUpdate :: AnswerHandler ()
checkForUpdate = do
  newLoc <- gets _answererTmpLocation
  oldLoc <- gets _answererOrigLocation
  logLoc <- gets _logLocation
  upRef <- gets _doUpdate
  upb <- liftIO $ readIORef upRef
  liftIO . when upb $
        moveInAnswerer logLoc oldLoc newLoc
  liftIO $ atomicWriteIORef upRef False
        
         
setUpdate :: AnswerHandler ()
setUpdate = do
  up <- gets _doUpdate
  liftIO $ atomicWriteIORef up True

askQuestion :: String -> AnswerHandler (Maybe String)
askQuestion str = do
  fp <- gets _answererTmpLocation
  liftIO $ do
    (inh,outh,proch) <- spawnProcess fp
    hSetBuffering inh NoBuffering
    hSetBuffering outh NoBuffering
    hPutStr inh str
    hPutStr inh "\n"
    reply <- hGetLine outh
    hClose inh
    hClose outh
    terminateProcess proch
    return (read reply)
    

logAnswerer :: String -> AnswerHandler ()
logAnswerer str = do 
  l <- gets _logLocation
  liftIO $ do
    writeFile l str
    writeFile l "\n"
