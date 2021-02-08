module Lib
    ( module Lib
    ) where

import qualified Algebra.Graph.Undirected as Undirected
import Algebra.Graph.Undirected ( Graph )
import qualified Data.Map as Map
import Data.Map ( Map )
import qualified Data.Set as Set
import Data.Set ( Set )
import Data.List ( find )
import Data.Maybe


-- STRICTLY UNCHANGING BOARD RELATED (Should we no use record syntax?)
data Province = Boh | Bud | Gal | Tri | Tyr | Vie | Cly | Edi | Lvp | Lon | Wal | Yor | Bre | Bur 
              | Gas | Mar | Par | Pic | Ber | Kie | Mun | Pru | Ruh | Sil | Apu | Nap | Pie | Rom 
              | Tus | Ven | Fin | Lvn | Mos | Sev | StP | Ukr | War | Ank | Arm | Con | Smy | Syr 
              | Alb | Bel | Bul | Den | Gre | Hol | Nwy | NAf | Por | Rum | Ser | Spa | Swe | Tun 
              | Adr | Aeg | Bal | Bar | Bla | Eas | Eng | Bot | GoL | Hel | Ion | Iri | Mid | NAt 
              | Nth | Nrg | Ska | Tyn | Wes | StPNC | StPSC | SpaNC | SpaSC | BulEC | BulSC
              deriving (Eq, Ord, Enum, Show)
type BoardGraph = Graph Province
data Country = England | Germany | Russia | Turkey | Italy | France | Austria | CivilDisorder deriving (Eq, Ord, Enum, Show)
data ProvinceType = Inland | Water | Coastal deriving (Eq, Ord, Enum, Show)
type Supply = Bool
data ProvinceData = ProvinceData {
        provinceType :: ProvinceType    -- What type of units go there
      , supply       :: Supply          -- If supply center is present
      , homeCountry  :: Maybe Country   -- What the home country of the supply center is
    } deriving (Eq, Show)
type BoardData = Map Province ProvinceData
-- data Board = Board BoardGraph BoardData deriving (Eq, Show)

graph :: BoardGraph
graph = Undirected.edges 
    [(NAt,Nrg),(NAt,Cly),(NAt,Lvp),(NAt,Iri),(NAt,Mid),(Nrg,Edi),(Nrg,Nth),(Nrg,Nwy),(Nrg,Bar)
    ,(Cly,Edi),(Cly,Lvp),(Lvp,Edi),(Lvp,Yor),(Lvp,Wal),(Lvp,Iri),(Iri,Wal),(Iri,Eng),(Iri,Mid)
    ,(Mid,Eng),(Mid,Bre),(Mid,Gas),(Mid,Spa),(Mid,Por),(Mid,Wes),(Mid,NAf),(Bar,StP),(Bar,Nwy)
    ,(Nwy,Fin),(Nwy,Swe),(Nwy,Ska),(Nwy,Nth),(Nth,Ska),(Nth,Den),(Nth,Hel),(Nth,Hol),(Nth,Bel)
    ,(Nth,Eng),(Nth,Lon),(Nth,Yor),(Nth,Edi),(Edi,Yor),(Yor,Lon),(Yor,Wal),(Wal,Lon),(Wal,Eng)
    ,(Eng,Lon),(Eng,Bel),(Eng,Pic),(Eng,Bre),(Bre,Pic),(Bre,Par),(Bre,Gas),(Gas,Par),(Gas,Bur)
    ,(Gas,Mar),(Gas,Spa),(Spa,Mar),(Spa,GoL),(Spa,Wes),(Spa,Por),(NAf,Wes),(NAf,Tun),(Tun,Wes)
    ,(Tun,Tyn),(Tun,Ion),(Wes,GoL),(Wes,Tyn),(GoL,Mar),(GoL,Pie),(GoL,Tyn),(Mar,Bur),(Mar,Pie)
    ,(Bur,Par),(Bur,Pic),(Bur,Bel),(Bur,Ruh),(Bur,Mun),(Pic,Par),(Pic,Bel),(Bel,Hol),(Bel,Ruh)
    ,(Hol,Hel),(Hol,Kie),(Hol,Ruh),(Hel,Den),(Hel,Kie),(Den,Ska),(Den,Swe),(Den,Bal),(Den,Kie)
    ,(Swe,Fin),(Swe,Ska),(Swe,Bot),(Swe,Bal),(Bot,Fin),(Bot,StP),(Bot,Lvn),(Bot,Bal),(StP,Fin)
    ,(StP,Lvn),(StP,Mos),(StP,Nwy),(Mos,Lvn),(Mos,War),(Mos,Ukr),(Mos,Sev),(Lvn,Bal),(Lvn,Pru),(Lvn,War)
    ,(Bal,Pru),(Bal,Ber),(Bal,Kie),(Kie,Ber),(Kie,Mun),(Kie,Ruh),(Ruh,Mun),(Ber,Pru),(Ber,Sil)
    ,(Ber,Mun),(Pru,War),(Pru,Sil),(Mun,Sil),(Mun,Boh),(Mun,Tyr),(Pie,Ven),(Pie,Tyr),(Pie,Tus)
    ,(War,Ukr),(War,Gal),(War,Sil),(Ukr,Sev),(Ukr,Rum),(Ukr,Gal),(Sev,Arm),(Sev,Bla),(Sev,Rum)
    ,(Boh,Sil),(Boh,Gal),(Boh,Vie),(Boh,Tyr),(Tyr,Vie),(Tyr,Tri),(Tyr,Ven),(Ven,Tri),(Ven,Adr)
    ,(Ven,Tus),(Tus,Rom),(Tus,Tyn),(Tyn,Rom),(Tyn,Nap),(Tyn,Ion),(Rom,Apu),(Rom,Nap),(Vie,Gal)
    ,(Vie,Bud),(Vie,Tri),(Tri,Bud),(Tri,Ser),(Tri,Alb),(Tri,Adr),(Adr,Alb),(Adr,Ion),(Adr,Apu)
    ,(Apu,Nap),(Nap,Ion),(Gal,Rum),(Gal,Bud),(Bud,Rum),(Bud,Ser),(Ion,Alb),(Ion,Gre),(Ion,Aeg)
    ,(Ion,Eas),(Alb,Ser),(Alb,Gre),(Ser,Rum),(Ser,Bul),(Ser,Gre),(Rum,Bla),(Rum,Bul),(Bul,Bla)
    ,(Bul,Con),(Bul,Aeg),(Bul,Gre),(Gre,Aeg),(Aeg,Con),(Aeg,Smy),(Aeg,Eas),(Bla,Arm),(Bla,Ank)
    ,(Bla,Con),(Con,Ank),(Con,Smy),(Ank,Arm),(Ank,Smy),(Arm,Syr),(Arm,Smy),(Smy,Syr),(Smy,Eas)
    ,(Eas,Syr),(SpaNC,Por),(SpaNC,Mid),(SpaNC,Gas),(SpaSC,Mar),(SpaSC,GoL),(SpaSC,Wes)
    ,(SpaSC,Mid),(SpaSC,Por),(StPNC,Bar),(StPNC,Nwy),(StPSC,Fin),(StPSC,Bot),(StPSC,Lvn)
    ,(BulEC,Rum),(BulEC,Bla),(BulEC,Con),(BulSC,Con),(BulSC,Aeg),(BulSC,Gre)]

converge :: Province -> Province
converge StPNC = StP
converge StPSC = StP
converge SpaNC = Spa
converge SpaSC = Spa
converge BulEC = Bul
converge BulSC = Bul
converge p     = p

boardData :: BoardData
boardData = Map.fromList [ (Boh, ProvinceData Inland False Nothing)
                    , (Bud, ProvinceData Inland True  (Just Austria))
                    , (Gal, ProvinceData Inland False Nothing)
                    , (Tri, ProvinceData Coastal True (Just Austria))
                    , (Tyr, ProvinceData Inland False Nothing)
                    , (Vie, ProvinceData Inland True (Just Austria))
                    , (Cly, ProvinceData Coastal False Nothing)
                    , (Edi, ProvinceData Coastal True (Just England))
                    , (Lvp, ProvinceData Coastal True (Just England))
                    , (Lon, ProvinceData Coastal True (Just England))
                    , (Wal, ProvinceData Coastal False Nothing)
                    , (Yor, ProvinceData Coastal False Nothing)
                    , (Bre, ProvinceData Coastal True (Just France))
                    , (Bur, ProvinceData Inland False Nothing)
                    , (Gas, ProvinceData Coastal False Nothing)
                    , (Mar, ProvinceData Coastal True (Just France))
                    , (Par, ProvinceData Inland True (Just France))
                    , (Pic, ProvinceData Coastal False Nothing)
                    , (Ber, ProvinceData Coastal True (Just Germany))
                    , (Kie, ProvinceData Coastal True (Just Germany))
                    , (Mun, ProvinceData Inland True (Just Germany))
                    , (Pru, ProvinceData Coastal False Nothing)
                    , (Ruh, ProvinceData Inland False Nothing)
                    , (Sil, ProvinceData Inland False Nothing)
                    , (Apu, ProvinceData Coastal False Nothing)
                    , (Nap, ProvinceData Coastal True (Just Italy))
                    , (Pie, ProvinceData Coastal False Nothing)
                    , (Rom, ProvinceData Coastal True (Just Italy))
                    , (Tus, ProvinceData Coastal False Nothing)
                    , (Ven, ProvinceData Coastal True (Just Italy))
                    , (Fin, ProvinceData Coastal False Nothing)
                    , (Lvn, ProvinceData Coastal False Nothing)
                    , (Mos, ProvinceData Inland True (Just Russia))
                    , (Sev, ProvinceData Coastal True (Just Russia))
                    , (StP, ProvinceData Inland True (Just Russia))
                    , (StPNC, ProvinceData Coastal False (Just Russia))
                    , (StPSC, ProvinceData Coastal False (Just Russia))
                    , (Ukr, ProvinceData Inland False Nothing)
                    , (War, ProvinceData Inland True (Just Russia))
                    , (Ank, ProvinceData Coastal True (Just Turkey))
                    , (Arm, ProvinceData Coastal False Nothing)
                    , (Con, ProvinceData Coastal True (Just Turkey))
                    , (Smy, ProvinceData Coastal True (Just Turkey))
                    , (Syr, ProvinceData Coastal False Nothing)
                    , (Alb, ProvinceData Coastal False Nothing)
                    , (Bel, ProvinceData Coastal True Nothing)
                    , (Bul, ProvinceData Inland True Nothing)
                    , (BulEC, ProvinceData Coastal False Nothing)
                    , (BulSC, ProvinceData Coastal False Nothing)
                    , (Den, ProvinceData Coastal True Nothing)
                    , (Gre, ProvinceData Coastal True Nothing)
                    , (Hol, ProvinceData Coastal True Nothing)
                    , (Nwy, ProvinceData Coastal True Nothing)
                    , (NAf, ProvinceData Coastal False Nothing)
                    , (Por, ProvinceData Coastal True Nothing)
                    , (Rum, ProvinceData Coastal True Nothing)
                    , (Ser, ProvinceData Inland True Nothing)
                    , (Spa, ProvinceData Inland True Nothing)
                    , (SpaNC, ProvinceData Coastal False Nothing)
                    , (SpaSC, ProvinceData Coastal False Nothing)
                    , (Swe, ProvinceData Coastal True Nothing)
                    , (Tun, ProvinceData Coastal True Nothing)
                    , (Adr, ProvinceData Water False Nothing)
                    , (Aeg, ProvinceData Water False Nothing)
                    , (Bal, ProvinceData Water False Nothing)
                    , (Bar, ProvinceData Water False Nothing)
                    , (Bla, ProvinceData Water False Nothing)
                    , (Eas, ProvinceData Water False Nothing)
                    , (Eng, ProvinceData Water False Nothing)
                    , (Bot, ProvinceData Water False Nothing)
                    , (GoL, ProvinceData Water False Nothing)
                    , (Hel, ProvinceData Water False Nothing)
                    , (Ion, ProvinceData Water False Nothing)
                    , (Iri, ProvinceData Water False Nothing)
                    , (Mid, ProvinceData Water False Nothing)
                    , (NAt, ProvinceData Water False Nothing)
                    , (Nth, ProvinceData Water False Nothing)
                    , (Nrg, ProvinceData Water False Nothing)
                    , (Ska, ProvinceData Water False Nothing)
                    , (Tyn, ProvinceData Water False Nothing)
                    , (Wes, ProvinceData Water False Nothing)
                    ]





-- STATE DATA
data UnitType = Army | Fleet deriving (Eq, Ord, Enum, Show)
data UnitData = UnitData {
        unitType :: UnitType
      , country :: Country
    } deriving (Eq, Show)
type Units = Map Province UnitData
type DislodgedUnits = Units
data Date = Spring Int | Fall Int  deriving (Eq, Ord, Show)
type Supplies = Map Province Country
data GameState = GameState Date Supplies Units deriving (Eq, Show)
data DislogedState = DislogedState Date Supplies Units DislodgedUnits deriving (Eq, Show)

-- ACTIONS ON THE STATE (ORDERS, RETREAT/DISBAND, AND ADJUST), ORDERS FOR OrderPhase
data Order = Hold Province | Move Province Province | Support Province Province Province | Convoy Province Province Province deriving (Eq, Ord, Show)
type Orders = [Order] 

-- RETREAT OR DISBAND UNITS FOR DislodgedPhase
data DislogedOrder = Disband Province | Retreat Province Province
type DislogedOrders = [DislogedOrder]

-- ADJUST UNITS (DISBAND AND BUILD) DONE AFTER FALL TURN DislodgedPhase
data AdjustUnit = Loose Province | Build Province UnitType Country
type AdjustUnits = [AdjustUnit]

-- should this be Monadil? Like an log of each action sucess and failure?
-- this is basically what I wrote last time, but only for a narrow case
resolveOrders :: GameState -> Orders -> DislogedState
resolveOrders (GameState date supplies units) orders = DislogedState date supplies' units' dislogedUnits'
    where units' = foldl resolve units orders
          dislogedUnits' = Map.empty
          supplies' = supplies
          resolve :: Units -> Order -> Units
          resolve units (Move provinceFrom provinceTo) = Map.insert provinceTo (units Map.! provinceFrom) (Map.delete provinceFrom units)
          resolve units _ = units

-- a1 :: Board -> Units -> Orders -> Units --(Units, DislogedUnits)?
-- a1 = undefined 

-- a2 :: Board -> Units -> Orders -> DislodgedUnits
-- a2 = undefined 

-- a3 :: Board -> Supplies -> Units -> Supplies
-- a3 = undefined 


--Disband
-- Any using can be disbanded
--Retreat
-- Units can retreat to an unoccupied adjacent province
-- Resolving coast issues?
resolveDislodgements :: DislogedState -> DislogedOrders -> GameState
resolveDislodgements dislogedState dislodgedOrders = 
    (\(DislogedState d s u du) -> GameState d s u) $ foldl resolve dislogedState dislodgedOrders
    where resolve :: DislogedState -> DislogedOrder -> DislogedState
          resolve (DislogedState date supplies units dislodgedUnits) 
                  (Disband province) = 
                      DislogedState date supplies (Map.delete province units) (Map.delete province dislodgedUnits)
          resolve (DislogedState date supplies units dislodgedUnits) 
                  (Retreat provinceFrom provinceTo) = 
                      DislogedState date supplies (Map.insert provinceTo (dislodgedUnits Map.! provinceFrom) units) (Map.delete provinceFrom dislodgedUnits)

--Building
-- Can only be done if SupplyCenters > Units
--                  and Placement in HomeProvince
--                  and Placement is Unoccupied
--                  and Placement is Controlled
--                  and if Fleet then Placement not Inland
--                  and Coast is Specifed (if required)
--Disbanding
-- Any unit may be disbanded
resolveAdjustments :: GameState -> AdjustUnits -> GameState
resolveAdjustments gameState adjustUnits = foldl resolve gameState adjustUnits
    where resolve :: GameState -> AdjustUnit -> GameState
          resolve (GameState date supplies units) 
                  (Loose province) = GameState date supplies (Map.delete province units)
          resolve (GameState date supplies units) 
                  (Build province unit country) = 
                      GameState date supplies (Map.insert province (UnitData unit country) units)

--Should I have seperate methods that update Date?

-- The state transitions are following
{-
1Spring -2Orders-> 3Spring -4Retreat&Disband-> 5Fall -6Orders-> 7Fall -8Retreat&Disband-> 9Fall -10GainOrLoose-> (repeat)

1 : SpringState
3 : SpringDislogedState
5 : FallState
7 : FallDislogedState
9 : FallAdjustState -- same as a regular state?

2 : StateToDisloged
4 : DislogedToState
6 : StateToDisloged
8 : DislogedToState(Adjust)
10 : AdjustState


State -> DislogedState
DislogedState -> State
AdjustState (done after fall) State -> State
-}



-- type Orders = []

-- rewrite in monad style, State or Writer?
findCycle :: (Eq a, Ord a) => (a -> a) -> a -> Set a
findCycle m n = Set.fromList 
    $ (\(s, ns) -> s : takeWhile (/= s) ns) 
    $ fromJust 
    $ find (\(s, ns) -> s `elem` ns) 
    $ iterate (\(s, ns) -> (m s, s : ns)) (n, [])

findCycles :: (Eq a, Ord a) => (a -> a) -> [a] -> Set (Set a)
findCycles m ns = Set.fromList $ map (findCycle m) ns


movesDiagram12 :: Map Province Province
movesDiagram12 = undefined --Map.fromList [(A,C),(B,E),(C,D),(E,D),(G,C),(F,G)]

-- >>> 7 * 94
-- 658

