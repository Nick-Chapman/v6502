
module Top (main) where

import qualified Sim (main)
import LoadLogic (getLogic)

main :: IO ()
main = do
  logic <- getLogic
  Sim.main logic
