module Lib
    ( module Lib
    ) where

import qualified Data.Map as Map -- .Strict as Map
import qualified Algebra.Graph.Undirected as Undirected
import qualified Algebra.Graph as Directed
import qualified Data.Set as Set
import Data.List (inits)
import Data.List.Unique (allUnique, repeatedBy, repeated)
import Data.Bifunctor (bimap)
--import qualified Algebra.Graph.Labelled as Labelled

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Node = A | B | C | D | E | F | G | H | I | J | K deriving (Eq, Ord, Enum, Show)

graph :: Undirected.Graph Node
graph = Undirected.edges $ 
    [(A,B),(B,C),(C,D),(D,A),(E,F),(F,G),(G,E),(H,I),(I,J)]
    ++ zip [(A)..] [(A)..] -- self edges


graphMap :: Map.Map Node Bool
graphMap = Map.fromList [(A, True), (B, True), (C, True), (D, False)
                        ,(E, True), (F, True), (G, True), (H, True), (I, True), (J, True)]

data Move = Hold | Move | Support | Convoy deriving (Eq, Ord)

-- it might be best to used a labeled edge graph
-- use adjacency map
moves :: Map.Map Node Node
moves = Map.fromList [(A,B),(B,C),(C,D),(E,F),(F,G),(G,E),(H,I),(I,J),(J,I),(K,K)]

moves2 = Map.fromList [(A,B),(C,B),(D,D),(E,F),(F,G),(G,E),(H,I),(I,J),(J,I),(K,K)]

-- moves2 = Directed.edges [(B,C)]
-- moves3 = Directed.edges [(C,D)]
-- moves4 = Directed.edges []

-- turn takes in the board
-- this method may not be extendable
--turn :: Directed.Graph Node -> Map.Map Node Bool -> Map.Map Node Bool
--turn moves state = Directed.foldg Map.empty (\node -> Map.singleton node ((Map.!) state node)) (<>) (\map1 map2 -> fmap not map1 <> fmap not map2) moves <> state

-- Map.Map Node Node :: Moves
-- Undirected.Graph Node :: The board graph
-- Map.Map Node Bool :: The current placmenet of things
-- Map.Map Node Bool :: The new placmenet of things
turn :: Map.Map Node Node -> Undirected.Graph Node -> Map.Map Node Bool -> Map.Map Node Bool
turn moves graph tfstate =
  if (not $ validTurn moves graph tfstate)
  then undefined
  else Map.mapWithKey singleNode tfstate
  where singleNode k b
          | Map.member k moves = not b
          | k `elem` Map.elems moves = not b
          | otherwise = b


-- this could be written more clearly
-- eventually give more error information
-- also some of these states are acceptable in terms of how the game
-- would actually be played
validTurn :: Map.Map Node Node -> Undirected.Graph Node -> Map.Map Node Bool -> Bool
validTurn moves graph tfstate =
     (Map.keysSet moves `Set.isSubsetOf` Map.keysSet tfstate)
     -- moves keys must be subset of tfstate keys
  && (Undirected.isSubgraphOf (mapToGraph moves) graph)
     -- moves must be subgraph of graph (should never fail)
  && (and $ fmap (tfstate Map.!) $ Map.keys moves)
     -- all arrow tails should be true
  where mapToGraph = Undirected.edges . Map.toList

findCycles :: Map.Map Node Node -> [Map.Map Node Node]
findCycles = undefined

-- find the lengthyest directed paths
findPaths :: Map.Map Node Node -> [Map.Map Node Node]
findPaths graph = undefined
  -- may also need to find components

-- Endomaps will probably simplify a lot of this, or fixedPoint
-- Make and Endomap out of map, then repeatedly apply lookup on the the whole
-- strucuture, until the Endomap is the same
-- That is probably the best way to do it, but we have it now so whatever
data Terminus = Node | Set Node

findTerminus :: Node -> Map.Map Node Node -> Node
findTerminus node moves = case Map.lookup node moves of
                            Nothing -> node
                            Just next -> findTerminus next moves

findTerminus' :: Map.Map Node Node -> Node -> [[Node]]
findTerminus' moves node = inits $ iterate (goToTerminus moves) node
  where goToTerminus moves node = case Map.lookup node moves of
                                    Nothing -> node
                                    Just next -> next

--theList = take 10 $ fmap Set.fromList $ findTerminus' moves B
--theList' node = takeWhile allUnique $ findTerminus' moves node

newList :: Map.Map Node Node -> Node -> [[Node]]
newList moves node = dropWhile null $ fmap repeated $ findTerminus' moves node

theTerminus :: Map.Map Node Node -> Node -> Set.Set Node
theTerminus moves node = Set.fromList $ fst $ head $ dropWhile (\(a,b) -> a /= b) $ zip rep $ tail rep
  where rep = newList moves node
--repeated

--turn :: Map.Map Node Node -> Undirected.Graph Node -> Map.Map Node Bool -> Map.Map Node Bool
allTerminuses :: Map.Map Node Node -> Set.Set (Set.Set Node)
allTerminuses moves = Set.map (theTerminus moves) $ Map.keysSet moves

-- Now we find the transpose of the mapping, in terms of directed graph
reverseMap :: Map.Map Node Node -> Directed.Graph Node
reverseMap moves = Directed.transpose $ Directed.edges $ Map.toList moves

-- so right now, the most important functions are reverseMap and allTerminuses
-- somehow we need to make a Map.Map Node Bool (I think)
-- we can assume all node with outdegree = 0 are False, rest are True
-- gaurenteed by the turn validator
nextTurn :: Map.Map Node Node -> Map.Map Node Bool
nextTurn moves = mconcat $ fmap getMap $ fmap Set.toList $ Set.toList $ allTerminuses moves 
    where
        terminalNodes = mconcat $ Set.toList $ allTerminuses moves
        revGraph = reverseMap moves
        revDirection = Map.fromListWith (<>) $ fmap (bimap id (: [])) $ filter (\(_,b) -> Set.notMember b terminalNodes) $ Directed.edgeList $ reverseMap moves
        revDirection' = Map.fromList $ Directed.adjacencyList $ revGraph
        outDegree map node = length $ case map Map.!? node of
                                        Nothing -> []
                                        Just ns -> ns
        nextTurnGo ableToMove node = case moves Map.!? node of
                                        Nothing -> if ableToMove then Map.singleton node False else Map.singleton node True
                                        Just n -> Map.singleton node True <> nextTurnGo ableToMove (moves Map.! node)
        nextTurnGo' ableToMove node = case revDirection' Map.! node of
                                        [] -> Map.singleton node (not ableToMove)
                                        [n] -> Map.singleton node ableToMove <> nextTurnGo' ableToMove n
                                        ns -> Map.singleton node True <> (mconcat $ fmap (nextTurnGo' False) ns)
        outOfCycleNodes ns = mconcat $ fmap (lookup revDirection) ns
            where lookup map key = case map Map.!? key of
                                        Nothing -> []
                                        Just n -> n 
        getMap cycleOrSin
            | Just (head cycleOrSin) == moves Map.!? (head cycleOrSin) = Map.singleton (head cycleOrSin) True
            | length cycleOrSin == 1 = nextTurnGo' True $ head cycleOrSin
            | all (\n -> outDegree revDirection' n == 1) cycleOrSin = Map.fromList $ fmap (flip (,) True) cycleOrSin
            | otherwise = (mconcat $ fmap (nextTurnGo' False) $ outOfCycleNodes cycleOrSin) <> (Map.fromList $ fmap (flip (,) True) cycleOrSin)
        --                    if length cycleOrSin == 1
        --                    then nextTurnGo True $ head cycleOrSin
        --                    else if 
        --nextTurnGo ableToMove node = case ableToMove of
        --                                False -> Map.singleton node True <> nextTurnGo False (moves Map.! node)
        --                                True -> Map.singleton node True <> nextTurnGo True (moves Map.! node)
                                        -- TODO: make singleton maps and <> together

--nextGraph :: Map.Map Node Bool -> Map.Map Node Bool -> Map.Map Node Bool
nextGraph moves graph = nextTurn moves <> graph
