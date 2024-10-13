{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query = CreatePotion String [String]
          | MixPotions String String
          | InspectPotion String
          | DiscardPotion String
          | Exit
          

-- | The instances are needed basically for tests
instance Eq Query where
  (CreatePotion pt1 ing1) == (CreatePotion pt2 ing2) = pt1 == pt2 && ing1 == ing2
  (MixPotions p1 p2) == (MixPotions p1' p2') = p1 == p1' && p2 == p2'
  (InspectPotion p1) == (InspectPotion p2) = p1 == p2
  (DiscardPotion p1) == (DiscardPotion p2) = p1 == p2
  Exit == Exit = True
  _ == _ = False

instance Show Query where
  show (CreatePotion potionType ingredients) = "CreatePotion " ++ potionType ++ " " ++ show ingredients
  show (MixPotions p1 p2) = "MixPotions " ++ p1 ++ " with " ++ p2
  show (InspectPotion potionType) = "InspectPotion " ++ potionType
  show (DiscardPotion potionType) = "DiscardPotion " ++ potionType
  show Exit = "Exit"

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery input = case words input of
    ("create":potionType:ingredients) -> Right $ CreatePotion potionType ingredients
    ("mix":potion1:"with":potion2:[]) -> Right $ MixPotions potion1 potion2
    ("inspect":potionType:[])         -> Right $ InspectPotion potionType
    ("discard":potionType:[])         -> Right $ DiscardPotion potionType
    ("exit":[])                       -> Right Exit
    _                                 -> Left "Invalid command format"
parseQuery _ = Left "Not implemented 2"

orElse :: Either String a -> Either String a -> Either String a
orElse (Left _) y = y
orElse x _ = x

parseChar :: Char -> String -> Either String (Char, String)
parseChar c[] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c(h:t) | c == h  =Right (c,t)
                 | otherwise = Left ([c] ++ "not found in " ++ (h:t))
-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State = State {
  potions :: [(String, [String])], --List  of (potion type, ingredients)
  logs :: [String]  --Command history
}

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State [] []

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: Query -> State -> State
stateTransition (CreatePotion potionType ingredients) s =
  let newPotion = (potionType, ingredients)
      updatedPotions = newPotion : potions s
      logEntry = "Created potion: " ++ potionType
  in s { potions = updatedPotions, logs = logEntry : logs s}
stateTransition (MixPotions potion1 potion2) s =
  let logEntry = "Mixed potions: " ++ potion1 ++ "with" ++ potion2
  in s {logs = logEntry : logs s}
stateTransition (DiscardPotion potionType) s =
  let updatedPotions = filter (\(p, _) -> p/= potionType) (potions s)
      logEntry = "Discarded potion: " ++ potionType
  in s {potions = updatedPotions, logs = logEntry : logs s }

stateTransition Exit s = s

