
module Logic where

data Line = LineA AssignDef | LineW WireDef | LineM MuxDef

data AssignDef = AssignDef { aLeft :: NodeId, _aRight :: Exp }
data WireDef = WireDef { wLeft :: WireId, _wRight :: Exp }
data MuxDef = MuxDef
  { width :: Int
  , name :: String
  , o :: NodeId
  , i :: Exp
  , s :: Vec
  , d :: Vec
  }

data Vec = Vec [Exp]

data Exp
  = ENode NodeId
  | EWire WireId
  | ENot Exp
  | EOr Exp Exp
  | EAnd Exp Exp
  | EXor Exp Exp
  | EIte Exp Exp Exp

newtype NodeId = NodeId { _unNodeId :: Int }
  deriving (Eq,Ord)

newtype WireId = WireId { _unWireId :: String }
  deriving (Eq,Ord)
