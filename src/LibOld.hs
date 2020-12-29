{-# LANGUAGE TupleSections #-}
module LibOld
    ( module LibOld
    ) where

import qualified Data.Map as Map
import qualified Algebra.Graph.Undirected as Undirected
import qualified Algebra.Graph as Directed
import qualified Data.Set as Set
import Data.List (inits, sort, group, delete)
import Data.List.Unique (allUnique, repeatedBy, repeated)
import Data.Bifunctor (bimap)
import Data.Monoid as Monoid
import Data.Map.Merge.Lazy as Merge
import Data.Maybe (fromMaybe)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Node = A | B | C | D | E | F | G | H | I | J | K deriving (Eq, Ord, Enum, Show)

graphDiagram12 :: Undirected.Graph Node
graphDiagram12 = Undirected.edges $ 
    [(H,A),(A,B),(H,I),(H,C),(A,C),(A,D),(B,D),(B,E),(I,C),(C,D),(D,E),(C,F),(C,G),(D,G),(F,G)]
    ++ zip [A ..] [A ..] -- self edges

graphMapDiagram12 :: Map.Map Node (Maybe Int)
graphMapDiagram12 = Map.fromList [(A,Just 1),(B,Just 2),(C,Just 3),(D, Nothing),(E,Just 4),(F,Just 5)
                                 ,(G, Just 6),(H, Nothing),(I, Nothing)]

-- a self move is a hold (should it always??)
data Move = Move | Support deriving (Eq, Ord, Show) --Hold | Move | Support | Convoy deriving (Eq, Ord)

--moves with support
tripleToNestedTuple :: (a,b,c) -> (a,(b,c))
tripleToNestedTuple (a,b,c) = (a,(b,c))

movesDiagram12 = Map.fromList $ fmap tripleToNestedTuple 
    [(A,C,Support),(B,E,Support),(C,D,Move),(E,D,Move),(G,C,Move),(F,G,Support)]


-- this could be written more clearly
-- eventually give more error information
-- also some of these states are acceptable in terms of how the game
-- would actually be played
validTurn :: Map.Map Node Node -> Undirected.Graph Node -> Map.Map Node Bool -> Bool
validTurn moves graph tfstate =
     (Map.keysSet moves `Set.isSubsetOf` Map.keysSet tfstate)
     -- moves keys must be subset of tfstate keys
  && Undirected.isSubgraphOf (mapToGraph moves) graph
     -- moves must be subgraph of graph (should never fail)
  && and ((tfstate Map.!) <$> Map.keys moves)
     -- all arrow tails should be true
  where mapToGraph = Undirected.edges . Map.toList


-- Endomaps will probably simplify a lot of this, or fixedPoint
-- Make and Endomap out of map, then repeatedly apply lookup on the the whole
-- strucuture, until the Endomap is the same
-- That is probably the best way to do it, but we have it now so whatever
data Terminus = Node | Set Node

findTerminus' :: Map.Map Node Node -> Node -> [[Node]]
findTerminus' moves node = inits $ iterate (goToTerminus moves) node
  where goToTerminus moves node = fromMaybe node (Map.lookup node moves)

newList :: Map.Map Node Node -> Node -> [[Node]]
newList moves node = dropWhile null $ repeated <$> findTerminus' moves node

theTerminus :: Map.Map Node Node -> Node -> Set.Set Node
theTerminus moves node = Set.fromList $ fst $ head $ dropWhile (uncurry (/=)) $ zip rep $ tail rep
  where rep = newList moves node

allTerminuses :: Map.Map Node Node -> Set.Set (Set.Set Node)
allTerminuses moves = Set.map (theTerminus moves) $ Map.keysSet moves

-- Now we find the transpose of the mapping, in terms of directed graph
reverseMap :: Map.Map Node Node -> Directed.Graph Node
reverseMap moves = Directed.transpose $ Directed.edges $ Map.toList moves

nextTurn orders state = mconcat $ fmap (getMap . Set.toList) $ Set.toList $ allTerminuses ordersWOLabels2
    where
        ordersWOLabels = fmap fst orders
        moves = fst <$> Map.filter ((==) Move . snd) orders -- get all move orders
        supports = fst <$> Map.filter ((==) Support .snd) orders -- get all support orders -- might not need
        -- delete the move which the support is helping to attack
        movesNotSupported k a = case moves Map.!? (moves Map.! a) of 
                                  Nothing -> moves
                                  Just n -> if n == k 
                                            then Map.delete (moves Map.! a) moves
                                            else moves
        -- delete those supports which are cut
        nonCutSupport = Map.filterWithKey (\k a -> k `notElem` Map.elems (movesNotSupported k a)) supports 
        supportPowerLevel = Map.fromList $ fmap (\xs -> (head xs, Monoid.Sum (length xs))) $ group $ sort $ Map.elems nonCutSupport
        movesWithSum = fmap (, Monoid.Sum 0) moves
        ordersWithSum = fmap (, Monoid.Sum 0) orders
        ordersWithSupport = Merge.merge Merge.preserveMissing Merge.dropMissing (Merge.zipWithMatched zipValues) ordersWithSum supportPowerLevel
            where zipValues k (n, s0) sn = (n, s0 <> sn) -- have the power level for each move
        ordersSupportPower = fmap (\(n, s) -> getSum s) ordersWithSupport -- Map Node Int, for all orders
        terminalNodes = mconcat $ Set.toList $ allTerminuses moves
        -- this terminus calculation should consider supports as self move
        ordersWOLabels2 = Map.mapWithKey (\n (n', ot) -> if ot == Support then n else n') orders
        revGraph2 = reverseMap ordersWOLabels2
        revDirection2 = Map.fromListWith (<>) $ fmap (bimap id (: [])) $ filter (\(_,b) -> Set.notMember b terminalNodes) $ Directed.edgeList $ reverseMap ordersWOLabels2
        revDirection2' = Map.fromList $ Directed.adjacencyList revGraph2
        outDegree map node = length $ fromMaybe [] (map Map.!? node)
        -- nextTurnGo' is where most of the logic to handle power levels will probably be
        -- Someone disloged units will need to be passed up, right now they will be deleted
        nextTurnGo' ableToMove node 
            = case revDirection2' Map.! node of -- change
                -- null case is fine, it should be able to move
                [] -> if ableToMove
                      then (Map.singleton node Nothing, [])
                      else (Map.singleton node (state Map.! node), []) 
                [n] | ableToMove -> (Map.singleton node (state Map.! n), [])
                                    <> nextTurnGo' ableToMove n
                    | ordersSupportPower Map.! n >= 1 ->
                                    (Map.singleton node (state Map.! n), []) 
                                    <> nextTurnGo' True n 
                                    <> (Map.empty, [(node, state Map.! node)]) 
                    | otherwise ->  (Map.singleton node (state Map.! node), []) 
                                    <> nextTurnGo' ableToMove n -- it cant move
                ns -> case maxNode ns of
                        -- CONDIONALLY CAUSES DISLODGEMENT
                        Just n -> (Map.singleton node (state Map.! n), []) 
                                  <> nextTurnGo' True n 
                                  <> mconcat (fmap (nextTurnGo' False) (delete n ns)) 
                                  <> (Map.empty, [(node, state Map.! node)])
                        Nothing -> if ableToMove -- standoff with all ns
                                   then (Map.singleton node Nothing, []) 
                                        <> mconcat (fmap (nextTurnGo' False) ns)
                                   else (Map.singleton node (state Map.! node), []) 
                                        <> mconcat (fmap (nextTurnGo' False) ns)

        maxNode ns = if length nodesWithMax == 1 then Just (head ns) else Nothing
            where
                nodesWithPower = Map.restrictKeys ordersWithSupport $ Set.fromList ns
                maxNumber = maximum (Map.elems nodesWithPower)
                nodesWithMax = filter (\(n,i) -> i == maxNumber) (Map.toList nodesWithPower)
        outOfCycleNodes2 ns = mconcat $ fmap (lookup revDirection2) ns
            where lookup map key = fromMaybe [] (map Map.!? key)
        getMap cycleOrSin
            -- self loop case
            | Just (head cycleOrSin) == ordersWOLabels2 Map.!? head cycleOrSin = 
                    (Map.singleton (head cycleOrSin) (state Map.! head cycleOrSin), [])
            -- a linear case A->B->C->...
            | length cycleOrSin == 1 = nextTurnGo' True $ head cycleOrSin
            -- A cycle case A->B->C->A where all can move
            | all (\n -> outDegree revDirection2' n == 1) cycleOrSin && length cycleOrSin /= 2 = 
                    (Map.fromList $ fmap (\n -> (n, state Map.! head (revDirection2' Map.! n))) cycleOrSin, [])
            -- A cycle case, but where the cicle cant move, eg A->B<->C
            | otherwise = mconcat $ nextTurnGo' False <$> outOfCycleNodes2 cycleOrSin


