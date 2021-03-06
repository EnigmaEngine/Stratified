module Stratified.Interface where
import Control.Monad.State
import Stratified.Types
import Stratified.Utils
import System.IO
import Data.Maybe

display :: Page -> StateT Schedule IO ()
display (Menu prompt actions errorPage) = do
  state <- get
  liftIO $ putStr $ fst $ runState prompt state
  liftIO $ hFlush stdout
  response <- liftIO $ getLine
  case lookup response actions of
      Just (_,page) -> display page
      Nothing -> display errorPage

display (Input prompts handler page) = do
  state <- get
  let evaledPrompts = map (\(p,r) -> (fst $ runState p state, r)) prompts
      repl pt@(p,t) = do
        liftIO (putStr p)
        liftIO (hFlush stdout)
        ui <- liftIO getLine
        case t ui of
          Just r -> return r
          _ -> repl pt
  responses <- mapM repl evaledPrompts
  handler responses
  display page

display (Response pageText page) = do
  state <- get
  liftIO $ putStrLn $ fst $ runState pageText state
  display page

display (Help page@(Menu _ actions _)) = do
  let commandHelp (command,(helpText,_)) = command ++ "\t" ++ helpText
  display $ Response (liftState $ unlines $ map commandHelp actions) page

display Quit = return ()
