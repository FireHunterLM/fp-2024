{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 (Query(..), parseQuery, State(..), emptyState, stateTransition, potions) 

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,
    testCase "Parsing case 1 - give a better name" $
      Lib2.parseQuery "" @?= (Left "Some error message"),
    testCase "Parsing case 2 - give a better name" $
      Lib2.parseQuery "o" @?= (Left "Some error message")
  ]
-- Test the parseQuery function
test_parseQuery :: IO ()
test_parseQuery = do
    putStrLn "Testing parseQuery..."
    print (parseQuery "create healing unicorn_hair and dragon_scale" == Right (CreatePotion "healing" ["unicorn_hair", "and", "dragon_scale"]))
    print (parseQuery "mix healing with mana" == Right (MixPotions "healing" "mana"))
    print (parseQuery "inspect strength" == Right (InspectPotion "strength"))
    print (parseQuery "discard invisibility" == Right (DiscardPotion "invisibility"))
    print (parseQuery "exit" == Right Exit)

-- Test the stateTransition function
test_stateTransition :: IO ()
test_stateTransition = do
    putStrLn "Testing stateTransition..."
    let state1 = emptyState
    let state2 = stateTransition (CreatePotion "healing" ["unicorn_hair", "dragon_scale"]) state1
    print (potions state2 == [("healing", ["unicorn_hair", "dragon_scale"])])
    
    let state3 = stateTransition (DiscardPotion "healing") state2
    print (null (potions state3))