import Test.Hspec

import qualified Algebra.Graph.Undirected as Undirected
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List ( find )
import Data.Maybe

import Lib


main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
-- main = putStrLn "Test suite not yet implemented"

diplomacyBoard :: Board
diplomacyBoard = Board (Undirected.edges $ 
    [(A,B),(A,C),(A,D),(B,D),(B,E),(C,D),(D,E),(C,F),(C,G),(D,G),(F,G)])
    (Map.fromList [(A,(Inland,False)),(B,(Inland,True)),(C,(Inland,False)),(D,(Inland,False)),(E,(Inland,False)),(F,(Inland,False)),(G,(Inland,True))])

state4 :: GameState
state4 = OrderPhase (Spring 1901) (Map.fromList [(B, England), (G, Russia)]) (Map.fromList [(C,(Army,England)),(E,(Army,Germany))])
action4 :: Orders
action4 = [Move C D, Move E D]