module Stratified.Utils where
import Control.Monad.State
import Stratified.Types
import Data.Time

liftState :: a -> State Schedule a
liftState = state . toFunc
  where toFunc l = \s -> (l,s)

isString str = if (not $ null str)
               then Just str
               else Nothing

isPleasantness str = case (reads str :: [(Pleasantness,String)]) of
                 [(x, "")] -> if -2 <= x && x <= 2
                                then Just str
                                else Nothing
                 _ -> Nothing

isInt str = case (reads str :: [(Int,String)]) of
            [(x, "")] -> Just str
            _ -> Nothing

isDate str = case (parseTimeM True defaultTimeLocale "%-m/%-d/%0Y" str) :: Maybe ZonedTime of
             Just _ -> Just str
             _ -> Nothing

isTime str = case (parseTimeM True defaultTimeLocale "%-I:%M" str) :: Maybe ZonedTime of
             Just _ -> Just str
             _ -> Nothing

isMeridiem str = case (parseTimeM True defaultTimeLocale "%p" str) :: Maybe ZonedTime of
                 Just _ -> Just str
                 _ -> Nothing
