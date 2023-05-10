
module NodeNames (isNamed,toName,toNumName,ofName) where

import qualified Data.Map as Map
import Logic (NodeId(..))

isNamed :: NodeId -> Bool
isNamed i@(NodeId{}) =
  case Map.lookup i m of
    Just{} -> True
    Nothing -> False
  where m = Map.fromList [ (NodeId i,s) | (s,i) <- pairs ]

toName :: NodeId -> String
toName i@(NodeId num) =
  case Map.lookup i m of
    Just s -> s
    Nothing -> "NODE-" ++ show num
  where m = Map.fromList [ (NodeId i,s) | (s,i) <- pairs ]

toNumName :: NodeId -> String
toNumName i@(NodeId num) =
  case Map.lookup i m of
    Just s -> show num ++ "-" ++ s
    Nothing -> show num ++ "-???"
  where m = Map.fromList [ (NodeId i,s) | (s,i) <- pairs ]

ofName :: String -> NodeId
ofName s =
  case Map.lookup s m of
    Just i -> NodeId i
    Nothing -> error (show ("ofName",s))

  where m = Map.fromList pairs

-- converted by hand from ../data/chip_6502_nodes.inc
pairs :: [(String,Int)]
pairs =
  [("vcc",657)
  ,("vss",558)
  ,("cp1",710)
  ,("cp2",943)

  ,("res",159)
  ,("rw",1156)
  ,("db0",1005)
  ,("db1",82)
  ,("db3",650)
  ,("db2",945)
  ,("db5",175)
  ,("db4",1393)
  ,("db7",1349)
  ,("db6",1591)
  ,("ab0",268)
  ,("ab1",451)
  ,("ab2",1340)
  ,("ab3",211)
  ,("ab4",435)
  ,("ab5",736)
  ,("ab6",887)
  ,("ab7",1493)
  ,("ab8",230)
  ,("ab9",148)
  ,("ab12",1237)
  ,("ab13",349)
  ,("ab10",1443)
  ,("ab11",399)
  ,("ab14",672)
  ,("ab15",195)
  ,("sync",539)
  ,("so",1672)
  ,("clk0",1171)
  ,("clk1out",1163)
  ,("clk2out",421)
  ,("rdy",89)
  ,("nmi",1297)
  ,("irq",103)

  ,("dpc11_SBADD",549)
  ,("dpc9_DBADD",859)

  ,("a0",737)
  ,("a1",1234)
  ,("a2",978)
  ,("a3",162)
  ,("a4",727)
  ,("a5",858)
  ,("a6",1136)
  ,("a7",1653)

  ,("y0",64)
  ,("y1",1148)
  ,("y2",573)
  ,("y3",305)
  ,("y4",989)
  ,("y5",615)
  ,("y6",115)
  ,("y7",843)

  ,("x0",1216)
  ,("x1",98)
  ,("x2",1)
  ,("x3",1648)
  ,("x4",85)
  ,("x5",589)
  ,("x6",448)
  ,("x7",777)

  ,("pcl0",1139)
  ,("pcl1",1022)
  ,("pcl2",655)
  ,("pcl3",1359)
  ,("pcl4",900)
  ,("pcl5",622)
  ,("pcl6",377)
  ,("pcl7",1611)
  ,("pch0",1670)
  ,("pch1",292)
  ,("pch2",502)
  ,("pch3",584)
  ,("pch4",948)
  ,("pch5",49)
  ,("pch6",1551)
  ,("pch7",205)

  ,("Reset0",67)
  ,("C1x5Reset",926)

  ,("idl0",1597)
  ,("idl1",870)
  ,("idl2",1066)
  ,("idl3",464)
  ,("idl4",1306)
  ,("idl5",240)
  ,("idl6",1116)
  ,("idl7",391)

  ,("sb0",54)
  ,("sb1",1150)
  ,("sb2",1287)
  ,("sb3",1188)
  ,("sb4",1405)
  ,("sb5",166)
  ,("sb6",1336)
  ,("sb7",1001)

  ,("adl0",413)
  ,("adl1",1282)
  ,("adl2",1242)
  ,("adl3",684)
  ,("adl4",1437)
  ,("adl5",1630)
  ,("adl6",121)
  ,("adl7",1299)

  ,("adh0",407)
  ,("adh1",52)
  ,("adh2",1651)
  ,("adh3",315)
  ,("adh4",1160)
  ,("adh5",483)
  ,("adh6",13)
  ,("adh7",1539)

  ,("idb0",1108)
  ,("idb1",991)
  ,("idb2",1473)
  ,("idb3",1302)
  ,("idb4",892)
  ,("idb5",1503)
  ,("idb6",833)
  ,("idb7",493)

  ,("abl0",1096)
  ,("abl1",376)
  ,("abl2",1502)
  ,("abl3",1250)
  ,("abl4",1232)
  ,("abl5",234)
  ,("abl6",178)
  ,("abl7",567)

  ,("abh0",1429)
  ,("abh1",713)
  ,("abh2",287)
  ,("abh3",422)
  ,("abh4",1143)
  ,("abh5",775)
  ,("abh6",997)
  ,("abh7",489)

  ,("s0",1403)
  ,("s1",183)
  ,("s2",81)
  ,("s3",1532)
  ,("s4",1702)
  ,("s5",1098)
  ,("s6",1212)
  ,("s7",1435)

  ,("ir0",328)
  ,("ir1",1626)
  ,("ir2",1384)
  ,("ir3",1576)
  ,("ir4",1112)
  ,("ir5",1329)
  ,("ir6",337)
  ,("ir7",1328)

  ,("clock1",1536)
  ,("clock2",156)
  ,("t2",971)
  ,("t3",1567)
  ,("t4",690)
  ,("t5",909)

  ,("alu0",401)
  ,("alu1",872)
  ,("alu2",1637)
  ,("alu3",1414)
  ,("alu4",606)
  ,("alu5",314)
  ,("alu6",331)
  ,("alu7",765)

  ,("alua0",1167)
  ,("alua1",1248)
  ,("alua2",1332)
  ,("alua3",1680)
  ,("alua4",1142)
  ,("alua5",530)
  ,("alua6",1627)
  ,("alua7",1522)

  ,("alub0",977)
  ,("alub1",1432)
  ,("alub2",704)
  ,("alub3",96)
  ,("alub4",1645)
  ,("alub5",1678)
  ,("alub6",235)
  ,("alub7",1535)
  ]
