{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A simple snaplet to handle the answerer executable.
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
-- Some handy io utilities
----------------------------------------------------------------------------

-- | Moves in the answerer from the given location to the new location.
moveInAnswerer :: 
 FilePath -> -- ^ The location of log file
 FilePath -> -- ^ The location of the executable
 FilePath -> -- ^ Where to move the executable
 IO ()
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

-- | Small utility to spawn a process with pipes to std_in and std_out.
spawnProcess fp = do
  (Just inh,Just outh,_,prc) <-
    createProcess (proc fp []){
      std_in = CreatePipe,
      std_out = CreatePipe}
  return (inh,outh,prc)

------------------------------------------------------------------------------
-- Make the Answerer Snaplet
-----------------------------------------------------------------------------

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

type AnswerHandler = Handler Answerer Answerer
------------------------------------------------------------------------------
-- The Answerer snaplet api

-- | Triggers an update if one has been requested
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
        
         
-- | set to update next time checkForUpdate is called
setUpdate :: AnswerHandler ()
setUpdate = do
  up <- gets _doUpdate
  liftIO $ atomicWriteIORef up True

-- | ask the answerer a question
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
    

-- | write the given string to the log
logAnswerer :: String -> AnswerHandler ()
logAnswerer str = do 
  l <- gets _logLocation
  liftIO $ do
    writeFile l str
    writeFile l "\n"
