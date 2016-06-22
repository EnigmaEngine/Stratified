module Stratified.Types where
import Control.Monad.State
import Data.Time

type PageText = State Schedule String
type HelpText = String
type Prompt = State Schedule String
type Response = String
type Action = (HelpText,Page)
type Actions = [(Response,Action)]
type Pleasantness = Double -- [-2..2] Zero is neutral, -2 is least pleasant and 2 is most pleasant

data Task = Task { name :: String, pleasantness :: Pleasantness, due :: ZonedTime, etc :: DiffTime, nibbles :: [Task] }

type Schedule = [Task]

--type Schedule = [String]

data Page = Menu Prompt Actions Page | Input [Prompt] ([Response] -> StateT Schedule IO ()) Page | Response PageText Page | Help Page | Quit
