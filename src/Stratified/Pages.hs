module Stratified.Pages where
import Control.Monad.State
import Stratified.Types
import Stratified.Utils
import Data.Time

welcome :: Page
welcome = Response
  (litState $ unlines [ "Welcome to Stratified!"
                          , ""
                          , "Stratified is a scheduling program that focuses on diluting monotonous tasks"
                          , "and maintaining motivators."
                          , "Stratified is a command line based application."
                          , "To navigate, enter a command, hit enter, and follow the prompts provided."
                          ])
  home

newTask :: Page
newTask = Input
  (map litState [ "Enter task name: "
                , "Enter task pleasantness: "
                , "Enter task length: "
                ])
  (\(n:p:l:_) -> do
    now <- liftIO $ getZonedTime
    state $ \s -> ((),s++[(Task n (read p) now (secondsToDiffTime . read $ l) [])]))
  (Response (litState "New task added.\n") home)

showTasks :: Page
showTasks = Response
  (state $ \s -> (unlines (map show s), s))
  home

home :: Page
home = Menu
  (litState "Command (? for help): ")
  [ ( "t", ("list current tasks", showTasks) )
  , ( "n", ("create a new task", newTask) )
  , ( "?", ("print this help", Help home) )
  , ( "q", ("quit the program", Quit) ) ]
  (Help home)
