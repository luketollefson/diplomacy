import Test.Hspec

import qualified Algebra.Graph.Undirected as Undirected
import qualified Data.Map as Map
import Data.Map ( fromList, empty )
import qualified Data.Set as Set
import Data.List ( find )
import Data.Maybe

import Lib


main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "Diagram 4" $ do
            resolveOrders state4 action4 `shouldBe` state4'
        it "Diagram 5" $ do
            resolveOrders state5 action5 `shouldBe` state5'
-- main = putStrLn "Test suite not yet implemented"

diplomacyBoard :: Board
diplomacyBoard = Board (Undirected.edges $ 
    [(A,B),(A,C),(A,D),(B,D),(B,E),(C,D),(D,E),(C,F),(C,G),(D,G),(F,G)])
    (Map.fromList [(A,(Inland,False)),(B,(Inland,True)),(C,(Inland,False)),(D,(Inland,False)),(E,(Inland,False)),(F,(Inland,False)),(G,(Inland,True))])

state4 :: GameState
state4 = OrderPhase (Spring 1901) (Map.fromList [(B, England), (G, Russia)]) (Map.fromList [(C,(Army,England)),(E,(Army,Germany))])
action4 :: Orders
action4 = [Move C D, Move E D]
state4' :: DislogedState
state4 = GameState (Spring 1901) 
         (fromList [(Ber, Germany), (War, Russia)]) 
         (fromList [(Ber,UnitData Army Germany),(War,UnitData Army Russia)])
action4 = [Move Ber Sil, Move War Sil]
state4' = DislogedState (Fall 1901) 
          (fromList [(Ber, Germany), (War, Russia)]) 
          (fromList [(Ber,UnitData Army Germany),(War,UnitData Army Russia)])
          empty

state5 :: GameState
action5 :: Orders
state5' :: DislogedState
state5 = GameState (Spring 1901) 
         (fromList [(Kie, Germany), (Ber, Germany), (War, Russia)]) 
         (fromList [(Kie, UnitData Army Germany), (Ber,UnitData Army Germany),(Pru,UnitData Army Russia)])
action5 = [Move Kie Ber, Move Ber Pru, Hold Pru]
state5' = DislogedState (Fall 1901) 
          (fromList [(Kie, Germany), (Ber, Germany), (War, Russia)]) 
          (fromList [(Kie, UnitData Army Germany), (Ber,UnitData Army Germany),(Pru,UnitData Army Russia)])
          empty