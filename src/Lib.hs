module Lib
    ( module Lib
    ) where

import qualified Data.Map as Map -- .Strict as Map
import qualified Algebra.Graph.Undirected as Undirected
import qualified Algebra.Graph as Directed
import qualified Data.Set as Set
import Data.List (inits, sort, group, delete)
import Data.List.Unique (allUnique, repeatedBy, repeated)
import Data.Bifunctor (bimap)
import Data.Monoid as Monoid --(getSum, Sum)
import Data.Map.Merge.Lazy as Merge
--import qualified Algebra.Graph.Labelled as Labelled

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Node = A | B | C | D | E | F | G | H | I | J | K deriving (Eq, Ord, Enum, Show)

graph :: Undirected.Graph Node
graph = Undirected.edges $ 
    [(A,B),(B,C),(C,D),(D,A),(E,F),(F,G),(G,E),(H,I),(I,J)]
    ++ zip [(A)..] [(A)..] -- self edges

graphDiagram12 :: Undirected.Graph Node
graphDiagram12 = Undirected.edges $ 
    [(H,A),(A,B),(H,I),(H,C),(A,C),(A,D),(B,D),(B,E),(I,C),(C,D),(D,E),(C,F),(C,G),(D,G),(F,G)]
    ++ zip [(A)..] [(A)..] -- self edges


graphMap :: Map.Map Node Bool
graphMap = Map.fromList [(A, True), (B, True), (C, True), (D, False)
                        ,(E, True), (F, True), (G, True), (H, True), (I, True), (J, True)]

graphMapJI :: Map.Map Node (Maybe Int)
graphMapJI = Map.fromList [(A, Just 1), (B, Just 2), (C, Just 3), (D, Nothing), (E, Just 4)
                          ,(F, Just 5), (G, Just 6), (H, Just 7), (I, Just 8), (J, Just 9)
                          ,(K, Just 10)]

graphMapDiagram12 :: Map.Map Node (Maybe Int)
graphMapDiagram12 = Map.fromList [(A,Just 1),(B,Just 2),(C,Just 3),(D, Nothing),(E,Just 4),(F,Just 5)
                                 ,(G, Just 6),(H, Nothing),(I, Nothing)]

-- a self move is a hold (should it always??)
data Move = Move | Support deriving (Eq, Ord, Show) --Hold | Move | Support | Convoy deriving (Eq, Ord)

-- it might be best to used a labeled edge graph
-- use adjacency map
-- I don't think 
moves :: Map.Map Node Node
moves = Map.fromList [(A,B),(B,C),(C,D),(E,F),(F,G),(G,E),(H,I),(I,J),(J,I),(K,K)]
moves2 = Map.fromList [(A,B),(B,C),(D,E),(E,C),(C,F),(F,G),(H,I),(I,J),(J,K),(K,H)]

--moves with support
tripleToNestedTuple :: (a,b,c) -> (a,(b,c))
tripleToNestedTuple (a,b,c) = (a,(b,c))

movesS = Map.fromList $ fmap tripleToNestedTuple 
    [(A,B,Move), (B,C,Move), (C,D,Move), (E,F,Move), (F,G,Move), (G,E,Move), (H,I,Move), (I,J,Move)
    ,(J,I,Move), (K,K,Move)]
                
movesDiagram12 = Map.fromList $ fmap tripleToNestedTuple 
    [(A,C,Support),(B,E,Support),(C,D,Move),(E,D,Move),(G,C,Move),(F,G,Support)]

graphMapJI2 :: Map.Map Node (Maybe Int)
graphMapJI2 = Map.fromList [(A, Just 1), (B, Just 2), (C, Just 3), (D, Just 4), (E, Just 5)
                           ,(F, Just 6), (G, Nothing), (H, Just 7), (I, Just 8), (J, Just 9)
                           ,(K, Just 10)]

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
--nextTurn moves state = mconcat $ fmap getMap $ fmap Set.toList $ Set.toList $ allTerminuses moves 
--nextTurn :: Map.Map Node Node -> Map.Map Node (Maybe Int) -> Map.Map Node (Maybe Int)
--mconcat $ fmap getMap $ fmap Set.toList $ Set.toList $ allTerminuses moves 
-- TODO make allTerminuses work on orders
nextTurn orders state = mconcat $ fmap getMap $ fmap Set.toList $ Set.toList $ allTerminuses ordersWOLabels2 
    where
        ordersWOLabels = fmap fst orders
        moves = fmap fst $ Map.filter ((==) Move . snd) orders -- get all move orders
        supports = fmap fst $ Map.filter ((==) Support .snd) orders -- get all support orders -- might not need
        revMoves = Map.fromList $ Directed.adjacencyList $ reverseMap moves
        revSupports = Map.fromList $ Directed.adjacencyList $ reverseMap supports
        movesNotSupported k a = case moves Map.!? (moves Map.! a) of -- delete the move which the support is helping to attack
                                  Nothing -> moves
                                  Just n -> if n == k 
                                            then Map.delete (moves Map.! a) moves
                                            else moves
        nonCutSupport = Map.filterWithKey (\k a -> not (k `elem` Map.elems (movesNotSupported k a))) supports -- delete those supports which are cut
        supportPowerLevel = Map.fromList $ fmap (\xs -> (head xs, Monoid.Sum (length xs))) $ group $ sort $ Map.elems nonCutSupport
        movesWithSum = fmap (\a -> (a, Monoid.Sum 0)) moves
        ordersWithSum = fmap (\a -> (a, Monoid.Sum 0)) orders
        movesWithSupport = Merge.merge Merge.preserveMissing Merge.dropMissing (Merge.zipWithMatched zipValues) movesWithSum supportPowerLevel
            where zipValues k (n, s0) sn = (n, s0 <> sn) -- have the power level for each move
        ordersWithSupport = Merge.merge Merge.preserveMissing Merge.dropMissing (Merge.zipWithMatched zipValues) ordersWithSum supportPowerLevel
            where zipValues k (n, s0) sn = (n, s0 <> sn) -- have the power level for each move
        -- movesSupportPower will be used to decide who wins standoffs
        movesSupportPower = fmap (\(n, s) -> getSum s) movesWithSupport -- make it the final thing
        ordersSupportPower = fmap (\(n, s) -> getSum s) ordersWithSupport -- make it the final thing, orders have all the power level, hence its easier to use
        terminalNodes = mconcat $ Set.toList $ allTerminuses moves -- good
        revGraph = reverseMap moves -- good
        revDirection = Map.fromListWith (<>) $ fmap (bimap id (: [])) $ filter (\(_,b) -> Set.notMember b terminalNodes) $ Directed.edgeList $ reverseMap moves -- good
        revDirection' = Map.fromList $ Directed.adjacencyList $ revGraph -- good
        -- this terminus calculation should consider supports as self move
        ordersWOLabels2 = Map.mapWithKey (\n (n', ot) -> if ot == Support then n else n') orders
        revGraph2 = reverseMap ordersWOLabels2 -- good
        revDirection2 = Map.fromListWith (<>) $ fmap (bimap id (: [])) $ filter (\(_,b) -> Set.notMember b terminalNodes) $ Directed.edgeList $ reverseMap ordersWOLabels2 -- good
        revDirection2' = Map.fromList $ Directed.adjacencyList $ revGraph2 -- good
        outDegree map node = length $ case map Map.!? node of -- good
                                        Nothing -> []         -- good
                                        Just ns -> ns         -- good
        -- nextTurnGo' is where most of the logic to handle power levels will probably be
        -- Someone disloged units will need to be passed up, right now they will be deleted
        nextTurnGo' ableToMove node = case revDirection2' Map.! node of -- change
                                        -- null case is fine, it should be able to more
                                        [] -> if ableToMove
                                              then Map.singleton node Nothing
                                              else Map.singleton node (state Map.! node) -- (not ableToMove) -- change -- doesn't reverse anywhere
                                        -- singleton case should be able to move if it can move, but maybe not, it will disloge a non movable one
                                        [n] -> if ableToMove
                                               then Map.singleton node (state Map.! n) <> nextTurnGo' ableToMove n -- it can stictly move
                                               else (if ordersSupportPower Map.! n >= 1
                                                     then Map.singleton node (state Map.! n) <> nextTurnGo' True n -- if the next power level is higher, then it can mve
                                                     -- CAUSES DISLODGEMENT
                                                     else Map.singleton node (state Map.! node) <> nextTurnGo' ableToMove n) -- it cant move & it's power level is <=
                                               -- <> nextTurnGo' ableToMove n -- change -- can over a linear
                                        -- most complicated case (maybe), if all have same powerlevel, standoff, otherwise the highest wins, and ones behind the highest can move
                                        ns -> case maxNode ns of
                                                -- CONDIONALLY CAUSES DISLODGEMENT
                                                Just n -> Map.singleton node (state Map.! n) <> nextTurnGo' True n <> (mconcat $ fmap (nextTurnGo' False) (delete n ns))
                                                Nothing -> if ableToMove -- standoff with all ns
                                                           then Map.singleton node Nothing <> (mconcat $ fmap (nextTurnGo' False) ns)
                                                           else Map.singleton node (state Map.! node) <> (mconcat $ fmap (nextTurnGo' False) ns)

        maxNode ns = if length nodesWithMax == 1 then Just (head ns) else Nothing
            where
                nodesWithPower = Map.restrictKeys ordersWithSupport $ Set.fromList ns
                maxNumber = maximum (Map.elems nodesWithPower)
                nodesWithMax = filter (\(n,i) -> i == maxNumber) (Map.toList nodesWithPower)
        outOfCycleNodes ns = mconcat $ fmap (lookup revDirection) ns -- good
            where lookup map key = case map Map.!? key of -- good
                                        Nothing -> []     -- good
                                        Just n -> n       -- good
        outOfCycleNodes2 ns = mconcat $ fmap (lookup revDirection2) ns -- good
            where lookup map key = case map Map.!? key of -- good
                                        Nothing -> []     -- good
                                        Just n -> n       -- good
        getMap cycleOrSin
            -- self loop case
            | Just (head cycleOrSin) == ordersWOLabels2 Map.!? (head cycleOrSin) = Map.singleton (head cycleOrSin) (state Map.! (head cycleOrSin)) -- case where A<->A -- changed -- self loop
            -- a linear case A->B->C->...
            | length cycleOrSin == 1 = nextTurnGo' True $ head cycleOrSin -- case where A->B->C -- change
            -- A cycle case A->B->C->A where all can move
            | all (\n -> outDegree revDirection2' n == 1) cycleOrSin && length cycleOrSin /= 2 = Map.fromList $ fmap (\n -> (n, state Map.! (head (revDirection2' Map.! n)))) cycleOrSin -- case where A->B->C->A -- changed
            -- A cycle case, but where the cicle cant move, eg A->B<->C
            | otherwise = (mconcat $ fmap (nextTurnGo' False) $ outOfCycleNodes2 cycleOrSin) <> (Map.fromList (fmap (\n -> (n, state Map.! n)) cycleOrSin)) -- case where A->B<->C -- change

--
