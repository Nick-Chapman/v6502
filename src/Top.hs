
module Top (main) where

import LoadLogic (getLogic)
import System.Environment (getArgs)
import qualified Sim1 (main)
import qualified Sim2 (main)

main :: IO ()
main = do
  config <- parse <$> getArgs
  run config

parse :: [String] -> Config
parse = \case
  [] -> Config { mode = Sim2 }
  ["sim1"] -> Config { mode = Sim1 }
  args ->
    error (show ("parse",args))

data Config = Config { mode :: Mode }
data Mode = Sim1 | Sim2

run :: Config -> IO ()
run Config{mode} = case mode of
  Sim1 -> do
    logic <- getLogic
    Sim1.main logic
  Sim2 -> do
    logic <- getLogic
    Sim2.main logic
    pure ()
