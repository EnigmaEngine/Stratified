module Stratified.Types where
import Control.Monad.State
import Data.Time
import Data.List

type PageText = State Schedule String
type HelpText = String
type Reader = String -> Maybe String
type Prompt = State Schedule String
type PromptR = (Prompt, Reader)
type Response = String
type Action = (HelpText,Page)
type Actions = [(Response,Action)]
type Pleasantness = Double -- [-2..2] Zero is neutral, -2 is least pleasant and 2 is most pleasant

data Task = Task { name :: String, pleasantness :: Pleasantness, due :: ZonedTime, etc :: DiffTime, nibbles :: [Task] }

type Schedule = [Task]

data Page = Menu Prompt Actions Page | Input [PromptR] ([Response] -> StateT Schedule IO ()) Page | Response PageText Page | Help Page | Quit

instance Show Task where
  show = showTask 0
    where hoursAndMins = undefined
          showTask level (Task name pleasantness due etc nibbles) =
            let ns = map (showTask $ level + 1) nibbles
            in intercalate "\n" $ map (replicate (4*level) ' '++)
                 [ "Name: " ++ name
                 , "Pleasantness: " ++ show pleasantness
                 , "Due: " ++ formatTime defaultTimeLocale "%A, %-I:%M%P %-m/%-d/%0Y" due
                 , "Estimated Time: " ++ (show $ round $ toRational etc)
                 ] ++ if (not $ null ns) then ["Nibbles: "] ++ ns else []
