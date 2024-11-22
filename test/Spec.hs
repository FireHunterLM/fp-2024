{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=), assertFailure )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 (Query(..), parseQuery, State(..), emptyState, stateTransition, potions) 

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,
      
    -- Parsing tests
    testCase "Parsing empty input gives error" $
      Lib2.parseQuery "" @?= Left "Empty input please try again",  -- Updated expected error message

    testCase "Parsing single character gives error" $
      Lib2.parseQuery "o" @?= Left "Only one character has been written try again",  -- Updated expected error message
      
    -- State transition tests
    testCase "Create potion and verify state" $
      case stateTransition emptyState (CreatePotion "healing" ["unicorn_hair", "dragon_scale"]) of
        Right (_, state2) -> potions state2 @?= [("healing", ["unicorn_hair", "dragon_scale"])]
        Left err -> assertFailure ("Unexpected error: " ++ err),
        
    testCase "Discard potion and verify empty state" $
      case stateTransition emptyState (CreatePotion "healing" ["unicorn_hair", "dragon_scale"]) of
        Right (_, state1) -> 
          case stateTransition state1 (DiscardPotion "healing") of
            Right (_, state2) -> null (potions state2) @?= True
            Left err -> assertFailure ("Unexpected error: " ++ err)
        Left err -> assertFailure ("Unexpected error: " ++ err),
        
    testCase "Chain commands and verify logs" $
      let chainQuery = Chain [CreatePotion "healing" ["unicorn_hair"], MixPotions "healing" "mana"] in
      case stateTransition emptyState chainQuery of
        Right (_, state4) -> logs state4 @?= ["Created potion: healing", "Mixed potions: healing with mana"]
        Left err -> assertFailure ("Unexpected error: " ++ err)
    testCase "Parsing case 1 - give a better name" $
      Lib2.parseQuery "" @?= (Left "Some error message"),
    testCase "Parsing case 2 - give a better name" $
      Lib2.parseQuery "o" @?= (Left "Some error message")
  ]

propertyTests :: TestTree
propertyTests = testGroup "some meaningful name"
  [
    QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  ]