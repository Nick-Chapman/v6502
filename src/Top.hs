
module Top (main) where

import LoadLogic (getLogic)
import System.Environment (getArgs)
import qualified Sim (main)

main :: IO ()
main = do
  config <- parse <$> getArgs
  run config

parse :: [String] -> Config
parse = \case
  [] -> Config { mode = Dev }
  ["sim1"] -> Config { mode = Sim1 }
  args ->
    error (show ("parse",args))

data Config = Config { mode :: Mode }
data Mode = Sim1 | Dev

run :: Config -> IO ()
run Config{mode} = case mode of
  Sim1 -> do
    logic <- getLogic
    Sim.main logic
  Dev -> do
    print "*dev*"
    pure ()
