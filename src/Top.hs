
module Top (main) where

import GetLogic (Version(Raw,Simp,Minimal))
import System.Environment (getArgs)
import qualified CBM (main)

main :: IO ()
main = do
  config <- parse <$> getArgs
  run config

parse :: [String] -> Config
parse = loop config0
  where
    loop acc = \case
      [] ->acc
      "raw":xs -> loop acc { version = Raw } xs
      "simp":xs -> loop acc { version = Simp } xs
      "min":xs -> loop acc { version = Minimal } xs
      "-steps":n:xs -> loop acc { steps = read n } xs
      args ->
        error (show ("parse",args))

    config0 = Config { version = Raw, steps = 100 }


data Config = Config { version :: Version, steps :: Int }

run :: Config -> IO ()
run Config{version,steps=n} = do
  CBM.main version n
