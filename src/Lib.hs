module Lib
    ( module Lib
    ) where

import qualified Algebra.Graph.Undirected as Undirected
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List ( find )
import Data.Maybe


-- New version: started 23 May 2020
-- Goals
--      Simplify, especailly the following
--          terminus finding (if possible), prehaps there is a fixpoint/endomap libray or math
--          nextTurn, can logic be pulled out to be resued on top level?
--          In nextTurn, the main part, can 84-108 be massivly simplified
--      Renaming, give better and more descriptive names (as well as removing temporary names)
--      Use data type more, see if things belong to typeclasses I might not do this right now, I don't fully know what the data for validation, water etc.
--      Comment the reasoning
--      Make imported things easier to use
-- Non-Goals
--      Validation, I still don't know enough about the problem
-- Style
--      don't get too wide or indented
--      Use Transparent vs terse code
--      Don't abuse abstractions
--      Commits describe changes
--  Math stuff
--      Fixed point (discrete fixed point)
--      Endomap
--      Invariant Promising!
--      Cycles and fixed points
--      Periodic point
--      Limit set (continous)
--      Limit cycle (continuous)

-- STRICTLY UNCHANGING BOARD RELATED (Should we no use record syntax?)
data Province = A | B | C | D | E | F | G deriving (Eq, Ord, Enum, Show)
type BoardGraph = Undirected.Graph Province
data ProvinceType = Inland | Water | Coastal -- how do I handle provinces with two coasts
type Supply = Bool
type BoardData = Map.Map Province (ProvinceType, Supply) -- The Bool represents supply center
data Coast = Coast1 | Coast2
type CoastData = Map.Map Province (Map.Map Coast [Province]) -- Adjacency map for coasts?
data Board = Board BoardGraph BoardData CoastData

-- STATE DATA
data Country = England | Germany | Russia | Turkey | Italy | France | Austria | CivilDisorder deriving (Eq, Ord, Enum, Show)
type Supplies = Map.Map Province Country
data Unit = Army | Fleet deriving (Eq, Ord, Enum, Show)
type Units = Map.Map Province (Unit, Country, Maybe Coast)
type DislodgedUnits = Map.Map Province (Unit, Country, Maybe Coast)
data Date = Spring Int | Fall Int  deriving (Eq, Ord, Show)
data GameState = OrderPhase Date Supplies Units | DislodgedPhase Date Supplies Units DislodgedUnits

-- ACTIONS ON THE STATE (ORDERS, RETREAT/DISBAND, AND ADJUST)
-- ORDERS FOR OrderPhase
data Order = Hold Province | Move Province Province | Support Province Province Province | Convoy Province Province Province deriving (Eq, Ord, Show)
type Orders = [Order]

-- RETREAT OR DISBAND UNITS FOR DislodgedPhase
data DislogedOrder = Disband Province | Retreat Province Province
type DislogedOrders = [DislogedOrder]

-- ADJUST UNITS (DISBAND AND BUILD) DONE AFTER FALL TURN DislodgedPhase
data AdjustUnit = Loose Province | Build Unit Province
type AdjustUnits = [AdjustUnit]







-- type Orders = []

-- rewrite in monad style, State or Writer?
findCycle :: (Eq a, Ord a) => (a -> a) -> a -> Set.Set a
findCycle m n = Set.fromList 
    $ (\(s, ns) -> s : takeWhile (/= s) ns) 
    $ fromJust 
    $ find (\(s, ns) -> s `elem` ns) 
    $ iterate (\(s, ns) -> (m s, s : ns)) (n, [])

findCycles :: (Eq a, Ord a) => (a -> a) -> [a] -> Set.Set (Set.Set a)
findCycles m ns = Set.fromList $ map (findCycle m) ns


movesDiagram12 :: Map.Map Province Province
movesDiagram12 = Map.fromList [(A,C),(B,E),(C,D),(E,D),(G,C),(F,G)]

