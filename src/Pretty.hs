
module Pretty (ppLine,ppAssignDef) where

import Data.List (intercalate)
import Logic (Line(..),MuxDef(..),AssignDef(..),WireDef(..),Vec(..),NodeId(..),Exp(..),WireId(..))

ppLine :: Line -> String
ppLine = \case
  LineA x -> ppAssignDef x
  LineW x -> ppWireDef x
  LineM x -> ppMuxDef x

ppAssignDef :: AssignDef -> String
ppAssignDef (AssignDef n e) =
  "assign " ++ ppNodeId 'o' n ++ " = " ++ ppExpTop e ++ ";"

ppWireDef :: WireDef -> String
ppWireDef (WireDef w e) =
  "wire " ++ ppWireId w ++ " = " ++ ppExpTop e ++ ";"

ppMuxDef :: MuxDef -> String
ppMuxDef MuxDef{width=w,name,o,i,s,d} =
  "MUX #(" ++ show w ++ ") " ++ name ++ " ("
  ++ ".o(" ++ ppNodeId 'o' o ++ "), "
  ++ ".i(" ++ ppExpTop i ++ "), "
  ++ ".s(" ++ ppVec s ++ "), "
  ++ ".d(" ++ ppVec d ++ ")"
  ++ ");"

ppVec :: Vec -> String
ppVec (Vec xs) = "{" ++ intercalate "," (map ppExpTop xs) ++ "}"

-- precidence (from tight to loose)
-- Not(1), And(2), Or(3), Xor(4), Ite(5)

ppExpTop :: Exp -> String
ppExpTop = ppExp 5

-- The bigger the precidence passed down, the less likely we are to bracket
ppExp :: Int -> Exp -> String
ppExp prec = \case
  ENode n -> ppNodeId 'i' n
  EWire w -> ppWireId w
  ENot x -> "~" ++ ppExp 1 x
  EAnd x y -> bracket 2 (ppExp 2 x ++ "&" ++ ppExp 2 y)
  EOr x y -> bracket 3 (ppExp 3 x ++ "|" ++ ppExp 3 y)
  EXor x y -> bracket 4 (ppExp 4 x ++ "^" ++ ppExp 4 y)
  EIte x y z -> bracket 5 (ppExp 4 x ++ " ? " ++ ppExp 4 y ++ " : " ++ ppExp 5 z)
  EConst bool -> if bool then "1" else "0"

  where
    -- the small the arg passed to bracket, the less like likely we bracket
    bracket me s =
      if me > prec then "(" ++ s ++ ")" else s

ppNodeId :: Char -> NodeId -> String
ppNodeId c (NodeId n) = [c] ++ "[" ++ show n ++ "]"

ppWireId :: WireId -> String
ppWireId (WireId s) = s
