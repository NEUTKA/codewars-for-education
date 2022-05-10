module TitleCase (titleCase) where

import Data.List (find)
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize (c:cs) = toUpper c : map toLower cs
capitalize     [] = []

titleCase :: String -> String -> String
titleCase minor = unwords . process . words
  where 
    process :: [String] -> [String]
    process [] = []
    process (w:ws) = capitalize w : map checkNotMinor ws

    checkNotMinor :: String -> String
    checkNotMinor w = maybe (capitalize w) id $ find (== normalize w) normalizedMinors
    
    normalizedMinors :: [String]
    normalizedMinors = map normalize (words minor)

    normalize :: String -> String
    normalize = map toLower