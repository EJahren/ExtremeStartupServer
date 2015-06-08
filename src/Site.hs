{-# LANGUAGE OverloadedStrings #-}
module Site
  ( answerer
  ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as B
import           Data.ByteString (ByteString)
import           Snap.Core
import           Snap.Snaplet
import           Control.Monad.Trans
------------------------------------------------------------------------------
import           AnswerSnaplet


------------------------------------------------------------------------------

-- | The handler which handles questions: /?q="question"
handleQuestion = do
 mq <- getParam "q"
 case mq of
   Nothing ->  logAnswerer "No question given!"
   (Just q) -> do
     answer <- askQuestion (B.unpack q)
     checkForUpdate
     case answer of
       Nothing    -> do
         logAnswerer ("Could not answer: " ++ show q)
         writeBS ""
       (Just ans) -> 
         writeBS (B.pack ans)

-- | The handler which handles /update
handleUpdate = do 
  setUpdate
  writeBS "Set to update\n"
------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AnswerHandler ())]
routes = [ ("/",handleQuestion),
           ("/update",handleUpdate)]


------------------------------------------------------------------------------
-- | The application initializer.
answerer :: SnapletInit Answerer Answerer
answerer = makeSnaplet "Answerer" "An snaplet for the Extreme Startup game" Nothing $ do
    addRoutes routes
    rv <- initAnswerer "config.cfg"
    case rv of
      (Left e) -> error ("Could not parse config:" ++ show e)
      (Right v) -> return v
