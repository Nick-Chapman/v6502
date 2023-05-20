
module Top (main) where

import LoadLogic (getLogic)
import System.Environment (getArgs)
import qualified Sim1 (main)
import qualified Sim2 (main)
import qualified Dormann (main)
import qualified CBM (main)
import Sim2 (Version(Raw,Simp,Minimal))

main :: IO ()
main = do
  config <- parse <$> getArgs
  run config

parse :: [String] -> Config
parse = loop config0
  where
    loop acc = \case
      [] ->acc
      "cbm":n:xs -> loop acc { mode = CBM (read n) } xs
      "dor":xs -> loop acc { mode = Dormann } xs
      "sim1":xs -> loop acc { mode = Sim1 } xs
      "sim2":xs -> loop acc { mode = Sim2 } xs
      "raw":xs -> loop acc { version = Raw } xs
      "simp":xs -> loop acc { version = Simp } xs
      "min":xs -> loop acc { version = Minimal } xs
      args ->
        error (show ("parse",args))

    config0 = Config { mode = Dormann, version = Raw }


data Config = Config { mode :: Mode, version :: Version }
data Mode = Sim1 | Sim2 | Dormann | CBM Int

run :: Config -> IO ()
run Config{mode,version} = case mode of
  Sim1 -> do
    logic <- getLogic
    Sim1.main logic
  Sim2 -> do
    Sim2.main version
    pure ()
  Dormann -> do
    Dormann.main version
    pure ()
  CBM n ->  do
    CBM.main n
    pure ()
