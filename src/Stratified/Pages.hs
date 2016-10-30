module Stratified.Pages where
import Control.Monad.State
import Stratified.Types
import Stratified.Utils
import Data.Time
import Data.Maybe

welcome :: Page
welcome = Response
  (liftState $ unlines [ "Welcome to Stratified!"
                       , ""
                       , "Stratified is a scheduling program that focuses on diluting monotonous tasks and maintaining motivators."
                       , "Stratified is a command line based application."
                       , "To navigate, enter a command, hit enter, and follow the prompts provided."
                       ])
  home

newTask :: Page
newTask = Input
  (zip (map liftState [ "Enter task name: "
                 , "Enter task pleasantness (-2 to 2): "
                 , "Enter due date (MM/DD/YYYY): "
                 , "Enter time due (HH:MM): "
                 , "AM or PM?: "
                 , "Enter task length: "
                 ])
  [isString,isPleasantness,isDate,isTime,isMeridiem,isInt])
  (\(n:p:d:t:a:l:_) -> do
    state $ \s -> ((),s++[(Task n (read p) (fromJust $ parseTimeM True defaultTimeLocale "%-I:%M%p %-m/%-d/%0Y" (t++a++" "++d)) (secondsToDiffTime . read $ l) [])]))
  (Response (liftState "New task added.\n") home)

showTasks :: Page
showTasks = Response
  (state $ \s -> (unlines (map show s), s))
  home

home :: Page
home = Menu
  (liftState "Command (? for help): ")
  [ ( "t", ("list current tasks", showTasks) )
  , ( "n", ("create a new task", newTask) )
  , ( "?", ("print this help", Help home) )
  , ( "q", ("quit the program", Quit) ) ]
  (Help home)
