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
          | Chain [Query]
splitOn' :: Char -> String -> [String]
splitOn' _ [] = []
splitOn' delimiter str =
    let (first, remainder) = break (== delimiter) str
    in first : case remainder of
        [] -> []
        (_:rest) -> splitOn' delimiter rest
        
          

-- | The instances are needed basically for tests
instance Eq Query where
  (CreatePotion pt1 ing1) == (CreatePotion pt2 ing2) = pt1 == pt2 && ing1 == ing2
  (MixPotions p1 p2) == (MixPotions p1' p2') = p1 == p1' && p2 == p2'
  (InspectPotion p1) == (InspectPotion p2) = p1 == p2
  (DiscardPotion p1) == (DiscardPotion p2) = p1 == p2
  Exit == Exit = True
  (Chain q1) == (Chain q2) = q1 == q2  
  _ == _ = False


instance Show Query where
  show (CreatePotion potionType ingredients) = "CreatePotion " ++ potionType ++ " " ++ show ingredients
  show (MixPotions p1 p2) = "MixPotions " ++ p1 ++ " with " ++ p2
  show (InspectPotion potionType) = "InspectPotion " ++ potionType
  show (DiscardPotion potionType) = "DiscardPotion " ++ potionType
  show Exit = "Exit"
  show (Chain queries) = "Chain " ++ show queries

-- | Parses user's input.
-- The function must have tests.
-- | Parses user's input using combinators and helpers.
parseQuery :: String -> Either String Query
parseQuery input
  | null input = Left "Some error message"
  | length input == 1 = Left "Some error message"
  | otherwise = parseCreate input
              `orElse` parseMix input
              `orElse` parseInspect input
              `orElse` parseDiscard input
              `orElse` if input == "exit" then Right Exit else Left "Invalid command format"

-- Parses "create" command
parseCreate :: String -> Either String Query
parseCreate input = case words input of
  ("create":potionType:ingredients) -> Right $ CreatePotion potionType ingredients
  _ -> Left "Invalid command format"

-- Parses "mix" command
parseMix :: String -> Either String Query
parseMix input = case words input of
  ("mix":potion1:"with":potion2:[]) -> Right $ MixPotions potion1 potion2
  _ -> Left "Invalid command format"

-- Parses "inspect" command
parseInspect :: String -> Either String Query
parseInspect input = case words input of
  ("inspect":potionType:[]) -> Right $ InspectPotion potionType
  _ -> Left "Invalid command format"

-- Parses "discard" command
parseDiscard :: String -> Either String Query
parseDiscard input = case words input of
  ("discard":potionType:[]) -> Right $ DiscardPotion potionType
  _ -> Left "Invalid command format"


parseChain :: [[String]] -> Either String Query
parseChain [] = Left "Empty chain"
parseChain cmds = do
    queries <- mapM (parseQuery . unwords) cmds
    Right $ Chain queries


orElse :: Either String a -> Either String a -> Either String a
orElse (Left _) y = y
orElse x _ = x

-- and2 combines two parsers
and2 :: Either String a -> Either String b -> Either String (a, b)
and2 (Right a) (Right b) = Right (a, b)
and2 (Left err) _ = Left err
and2 _ (Left err) = Left err

-- and3 combines three parsers
and3 :: Either String a -> Either String b -> Either String c -> Either String (a, b, c)
and3 (Right a) (Right b) (Right c) = Right (a, b, c)
and3 (Left err) _ _ = Left err
and3 _ (Left err) _ = Left err
and3 _ _ (Left err) = Left err

-- Parses a single character
parseChar :: Char -> String -> Either String (Char, String)
parseChar c (h:t)
  | c == h    = Right (h, t)
  | otherwise = Left $ "Expected " ++ [c] ++ " but found " ++ [h]
parseChar c [] = Left $ "Expected " ++ [c] ++ " but found end of input"

-- Parses a string
parseString :: String -> String -> Either String String
parseString [] input = Right input
parseString (h:t) input = case parseChar h input of
  Right (_, rest) -> parseString t rest
  Left err -> Left err

-- Parses a digit (basic example)
parseDigit :: String -> Either String (Char, String)
parseDigit (h:t)
  | h `elem` ['0'..'9'] = Right (h, t)
  | otherwise = Left $ "Expected a digit but found " ++ [h]
parseDigit [] = Left "Expected a digit but found end of input"

-- Parses whitespace
parseWhitespace :: String -> Either String ((), String)
parseWhitespace (' ':t) = Right ((), t)
parseWhitespace ('\t':t) = Right ((), t)
parseWhitespace _ = Left "Expected whitespace"
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
stateTransition :: State -> Query -> Either String (Maybe String, State)

stateTransition s (CreatePotion potionType ingredients) =
  let newPotion = (potionType, ingredients)
      updatedPotions = newPotion : potions s
      logEntry = "Created potion: " ++ potionType
  in Right (Just logEntry, s { potions = updatedPotions, logs = logs s ++ [logEntry] })

stateTransition s (MixPotions potion1 potion2) =
  let logEntry = "Mixed potions: " ++ potion1 ++ " with " ++ potion2
  in Right (Just logEntry, s { logs = logs s ++ [logEntry] })

stateTransition s (InspectPotion potionType) =
  let logEntry = "Inspected potion: " ++ potionType
  in Right (Just logEntry, s { logs = logs s ++ [logEntry] })

stateTransition s (DiscardPotion potionType) =
  let updatedPotions = filter (\(p, _) -> p /= potionType) (potions s)
      logEntry = "Discarded potion: " ++ potionType
  in Right (Just logEntry, s { potions = updatedPotions, logs = logs s ++ [logEntry] })

stateTransition s (Chain queries) = foldl transitionHelper (Right (Nothing, s)) queries
  where
    transitionHelper (Right (_, st)) q = stateTransition st q
    transitionHelper (Left err) _ = Left err

stateTransition s Exit = Right (Nothing, s)


