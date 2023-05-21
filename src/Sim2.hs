
module Sim2
  ( Sim(..), simGivenLogic
  ) where

import Assigns (Exp(..))
import EmuState (State,makeState,lookState,getRW,getAB,getDB,Inputs,fixedInputs,resetLO,resetHI,posClk,negClk,setInputByte,applyInputs)
import GetLogic (Logic(..))
import Values (Addr,Byte)
import qualified Data.Map as Map

data Sim
  = Stabilization (Maybe Int) Sim
  | NewState State Sim
  | Decide Addr CycleKind Sim
  | ReadMem Addr (Byte -> Sim)
  | WriteMem Addr Byte Sim

data CycleKind = ReadCycle | WriteCycle deriving Show

-- When clock is high: Addr & R/W line are changed by 6502
-- When clock is low: Data(Byte) is changed, by 6502 (Write), by Mem (Read)

-- So new address (& R/W) appear after negative clock edges
-- And new data-bytes are writen-to or read-from memory on positive clock edges

simGivenLogic :: Logic -> Sim
simGivenLogic logic = do
  let s0 = initState logic
  stabDuringResetPermissive (posClk++extra) s0 $ \s1 -> do
  stabDuringReset (negClk) s1 $ \s2 -> do
  stabDuringReset (posClk) s2 $ \s3 -> do
  stabDuringReset (negClk) s3 $ \s4 -> do
  loop s4

  where
    -- hack to match perfect6502
    extra =
      []
      ++ setInputByte "s" 0xC0
      ++ setInputByte "x" 0xC0

    loop :: State -> Sim
    loop s0 = do
      -- We collect the r/w line and address bus after a neg clock edge
      let addr = getAB s0
      let kind = if (getRW s0) then ReadCycle else WriteCycle
      Decide addr kind $ do
      case kind of

        ReadCycle -> do
          -- Present the byte read from memory when clock is High.
          ReadMem addr $ \byte -> do
          stab (posClk ++ setInputByte "db" byte) s0 $ \s1 -> do
          NewState s1 $ do
          stab (negClk) s1 $ \s2 -> do
          NewState s2 $ do
          loop s2

        WriteCycle -> do
          -- Collect the byte to be written to memory when clock is High.
          stab posClk s0 $ \s1 -> do
          NewState s1 $ do
          WriteMem addr (getDB s1) $ do
          stab negClk s1 $ \s2 -> do
          NewState s2 $ do
          loop s2

    stab                      = stabG Strict resetHI
    stabDuringReset           = stabG Strict resetLO
    stabDuringResetPermissive = stabG Permissive resetLO

    stabG :: StabMode -> Inputs -> Inputs -> State -> (State -> Sim) -> Sim
    stabG mode reset i s k = do
      let (iopt,s') = stabilize mode logic (fixedInputs ++ reset ++ i) s
      Stabilization iopt $ do
      k s'

initState :: Logic -> State
initState (Logic{m}) = makeState [ (n,False) | (n,_) <- Map.toList m ]

data StabMode = Strict | Permissive

stabilize :: StabMode -> Logic -> Inputs -> State -> (Maybe Int,State)
stabilize mode logic inputs s0 = loop 0 (applyInputs inputs s0)
  where
    max = 50
    err = error (show ("failed to stabilize in",max))
    loop :: Int -> State -> (Maybe Int, State)
    loop i s1 = do
      let s2 = oneStep logic inputs s1
      if s1 == s2 then (Just i, s1) else
        if i == max then (case mode of Strict -> err; Permissive -> (Nothing,s1)) else
          loop (i+1) s2

oneStep :: Logic -> Inputs -> State -> State
oneStep Logic{m} inputs s0 = applyInputs inputs s'
  where
    s' = makeState [ (n,eval e) | (n,e) <- Map.toList m ]

    eval :: Exp -> Bool
    eval = \case
      ENode n -> lookState s0 n
      EWire{} -> error "eval/wire"
      EConst b -> b
      ENot x -> not (eval x)
      EAnd x y -> eval x && eval y
      EOr x y -> eval x || eval y
      EXor x y -> eval x /= eval y
      EIte x y z -> eval (if eval x then y else z)
