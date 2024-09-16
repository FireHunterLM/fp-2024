module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = 
    [ "create", "mix", "inspect", "discard", "exit"
    , "healing", "mana", "strength", "invisibility"
    , "unicorn_hair", "dragon_scale", "phoenix_feather", "nightshade", "mandrake_root"
    , "and", "with"
    ]
