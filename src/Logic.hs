
module Logic where

data Line = LineA AssignDef | LineW WireDef | LineM MuxDef

data AssignDef = AssignDef { aLeft :: NodeId, aRight :: Exp }
data WireDef = WireDef { wLeft :: WireId, wRight :: Exp }
data MuxDef = MuxDef
  { width :: Int
  , name :: String
  , o :: NodeId
  , i :: Exp
  , s :: Vec
  , d :: Vec
  }

data Vec = Vec { unVec :: [Exp] }

data Exp
  = ENode NodeId
  | EWire WireId
  | ENot Exp
  | EOr Exp Exp
  | EAnd Exp Exp
  | EXor Exp Exp
  | EIte Exp Exp Exp

newtype NodeId = NodeId { unNodeId :: Int }
  deriving (Eq,Ord)

newtype WireId = WireId { unWireId :: String }
  deriving (Eq,Ord)

instance Show WireId where
  show (WireId s) = "wire:" ++ s

instance Show NodeId where
  show (NodeId n) = "[" ++ show n ++ "]"
