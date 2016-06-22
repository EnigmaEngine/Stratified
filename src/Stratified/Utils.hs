module Stratified.Utils where
import Control.Monad.State
import Stratified.Types
import Data.Time
import Data.List

litState :: a -> State Schedule a
litState = state . toFunc
  where toFunc l = \s -> (l,s)

instance Show Task where
  show = showTask 0
    where hoursAndMins = undefined
          showTask level (Task name pleasantness due etc nibbles) =
            let ns = map (showTask $ level + 1) nibbles
            in intercalate "\n" $ map (replicate (4*level) ' '++)
                 [ "Name: " ++ name
                 , "Pleasantness: " ++ show pleasantness
                 , "Due: " ++ formatTime defaultTimeLocale "%A, %-I:%M%P %-m/%-d/%-y" due
                 , "Estimated Time: " ++ (show $ round $ toRational etc)
                 ] ++ if (not $ null ns) then ["Nibbles: "] ++ ns else []
