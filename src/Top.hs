
module Top (main) where

import EmuState (Sim)
import GetLogic (Version(Raw,Simp,Minimal),Logic,getLogic)
import Sim2 (simGivenLogic)
import System.Environment (getArgs)
import qualified CBM (main)
import qualified Compile (main)

main :: IO ()
main = do
  config <- parse <$> getArgs
  run config

parse :: [String] -> Config
parse = loop config0
  where
    loop acc = \case
      [] ->acc

      -- what system? CBM or just compiling
      "cbm":xs -> loop acc { sys = CBM } xs
      "seecompile":xs -> loop acc { sys = SeeCompile } xs

      -- logic version
      "raw":xs -> loop acc { version = Raw } xs
      "simp":xs -> loop acc { version = Simp } xs
      "minimal":xs -> loop acc { version = Minimal } xs

      -- which emulation mode
      "sim2":xs -> loop acc { mode = Sim2 } xs
      "dev":xs -> loop acc { mode = DevCompiledSim } xs

      -- controlling the emulation
      "-max":max:xs -> loop acc { max = read max } xs
      "-trace":xs -> loop acc { trace = True } xs

      args ->
        error (show ("parse",args))

    config0 = Config
      { sys = CBM
      , mode = DevCompiledSim
      , version = Minimal
      , max = 10
      , trace = False
      }

data Config = Config
  { sys :: Sys
  , mode :: Mode
  , version :: Version
  , max :: Int
  , trace :: Bool
  }

data Mode
  = Sim2 -- original simulation of set of mutually-dep assigns
  | DevCompiledSim -- simulation of linearlzed bindings

data Sys = CBM | SeeCompile

run :: Config -> IO ()
run Config{sys,mode,version,max,trace} = do
  case sys of
    SeeCompile -> Compile.main version
    CBM -> do
      logic <- getLogic version
      let sim = getSim logic mode
      CBM.main sim max trace

getSim :: Logic -> Mode -> Sim
getSim logic = \case
  Sim2 -> Sim2.simGivenLogic logic
  DevCompiledSim -> undefined
