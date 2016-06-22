module Main where
import Control.Monad.State
import Stratified.Interface
import Stratified.Types
import Stratified.Pages

main :: IO ((),Schedule)
main = runStateT (display welcome) [] 
