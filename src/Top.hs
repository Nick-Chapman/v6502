
module Top (main) where

import Compile (compile)
import GetLogic (Version(Raw,Simp,Min),getLogic)
import Misc (generateFile)
import System.Environment (getArgs)
import qualified CBM (main)
import qualified Sim2 (simGivenLogic)
import qualified Sim3 (simGivenProg)

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
      -- "cbm":xs -> loop acc { sys = CBM } xs

      -- logic version
      "raw":xs -> loop acc { version = Raw } xs
      "simp":xs -> loop acc { version = Simp } xs
      "min":xs -> loop acc { version = Min } xs

      -- which emulation mode
      "sim2":xs -> loop acc { mode = Sim2 } xs
      "sim3":xs -> loop acc { mode = Sim3 } xs

      -- controlling the emulation
      "-max":max:xs -> loop acc { max = read max } xs
      "-trace":xs -> loop acc { trace = True } xs
      "-beat":xs -> loop acc { heartbeat = True } xs

      args ->
        error (show ("parse",args))

    config0 = Config
      { sys = CBM
      , mode = Sim2
      , version = Min
      , heartbeat = False
      , max = 10
      , trace = False
      }

data Config = Config
  { sys :: Sys
  , mode :: Mode
  , version :: Version
  , heartbeat :: Bool
  , max :: Int
  , trace :: Bool
  }

data Mode
  = Sim2 -- original simulation of set of mutually-dep assigns
  | Sim3 -- simulation of linearlzed bindings

data Sys = CBM

run :: Config -> IO ()
run Config{sys,mode,version,heartbeat,max,trace} = do
  case sys of
    CBM ->
      case mode of
        Sim2 -> do
          logic <- getLogic version
          sim <- pure $ Sim2.simGivenLogic logic
          CBM.main sim heartbeat max trace
        Sim3 -> do
          logic <- getLogic version
          prog <- compile logic
          generateFile ("prog-"++show version) prog
          sim <- pure $ Sim3.simGivenProg prog
          CBM.main sim heartbeat max trace
