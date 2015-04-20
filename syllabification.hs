{-
Author: Bridget O'Daniel
Purpose: To print the syllables and stress of a provided Spanish word
Date: 6 April 2015

Acknowledgements:
* replaceAtIndex from: http://stackoverflow.com/questions/10133361/haskell-replace-element-in-list
* Information on Spanish syllabification from: http://www.spanishdict.com/topics/show/117

Errors: Does not properly syllabify words with prefixes such as in-, im-, and des-.
-}

module Main where
import Data.Char (toLower, toUpper)
import Data.Maybe (fromJust)
import Data.List (elemIndex)

-------------- Important Defined Things ----------------------

vowels =["a", "á", "e", "é", "i", "í", "o", "ó", "u", "ú", "ü"]

consonants = ["b", "c", "ch", "d", "f", "g", "h", "j", "k", "l", "ll", "m", "n", "ñ", "p", "q", "r", "rr"
             ,"s", "t", "v", "w", "x", "y", "z"]

cons_groups = ["bl", "br", "ch", "cl", "cr", "dr", "fl", "fr", "gl", "gr", "ll", "pl", "pr", "rr", "tr"]

accented_vowels = ["á", "é", "í", "ó", "ú"]

{- Categories:
W = Weak vowel
S = Strong vowel
G = Part of a consonant group
C = Consonant
C' = 's' (needed due to a rule: CC'C => "CC'- C" where - is the syllable break)-}
data Category = W | S | G | C | C'
    deriving (Show, Eq, Ord)

-------------- Predicates ----------------------

isVowel :: String -> Bool
isVowel a
    | a `elem` vowels = True
    | otherwise = False

isConsonant :: String -> Bool
isConsonant a = not (isVowel a)

isConsonantGroup :: String -> Bool
isConsonantGroup [] = False
isConsonantGroup (x:[]) = False
isConsonantGroup (x1:x2:xs)
    | [x1,x2] `elem` cons_groups = True
    | otherwise = False

{- Is the first character of the String part of a Consonant Group? -}
isFirstCharPartOfCG :: String -> Bool
isFirstCharPartOfCG s@(x:xs)
    | xs == [] = False
    | (isConsonantGroup ([x]++[(head xs)])) = True
    | otherwise = False

isStrong :: String -> Bool
isStrong "i" = False
isStrong "u" = False
isStrong "ü" = False
isStrong _ = True

isWeak :: String -> Bool
isWeak a = not (isStrong a)

isCharAccented :: Char -> Bool
isCharAccented c
    | [c] `elem` accented_vowels = True
    | otherwise = False

isAccented :: String -> Bool
isAccented a
    | True `elem` map isCharAccented a = True
    | otherwise = False

--Aguda: Stress on final syllable
isAguda :: [String] -> Bool
isAguda s
    | l == 'n' = False
    | l == 's' = False
    | isVowel [l] = False
    | otherwise = True
    where l = (last (last s))

--Grave: Stress on penultimate syllable
isGrave :: [String] -> Bool
isGrave s = not (isAguda s)

---------- Conversions -----------------------------

lower :: String -> String
lower = map toLower

upper :: String -> String
upper = map toUpper

{- Takes a String and creates a list containing the Category of each Char
   in that String -}
stringToCategoryList :: [Char] -> [Category]
stringToCategoryList [] = []
stringToCategoryList s@(x:xs)
    | isFirstCharPartOfCG s = (charToCategory x True) : (charToCategory (head xs) True) : (stringToCategoryList (tail xs))
    | otherwise = (charToCategory x False) : (stringToCategoryList xs)


{- Converts a Char to its appropriate Category and returns it. -}
charToCategory :: Char -> Bool-> Category
charToCategory c b
    | isWeak [c] = W
    | isVowel [c] = S
    | c == 's' = C'
    | b = G
    | otherwise = C

---------- Syllable Creation ------------------------

syllabify :: String -> String
syllabify [] = ""
syllabify a = hyphenate (stressify (toSyllableList a))

hyphenate :: [String] -> String
hyphenate [] = []
hyphenate (x:[]) = x
hyphenate (x:xs) = x ++ "-" ++ (hyphenate xs)

getFirstSyllable :: String -> String
getFirstSyllable [] = []
getFirstSyllable a = take len_first_syll a
    where len_first_syll = (length (getFirstCategorySyllable (stringToCategoryList a)))

toSyllableList :: String -> [String]
toSyllableList [] = []
toSyllableList (x:[]) = [[x]]
toSyllableList a = getFirstSyllable a : toSyllableList (drop (length (getFirstSyllable a)) a)

{- Gets the first syllable of the word as represented as
  a Category list. -}
getFirstCategorySyllable :: [Category] -> [Category]
getFirstCategorySyllable [] = []
getFirstCategorySyllable (c:[]) = [c]                                              --Last letter? It's a syllable by itself.
getFirstCategorySyllable (c1:c2:c3:xs)
    | (c1 == W || c1 == S) && (c2 == C || c2 == C') && (c3 == W || c3 == S) = [c1] --Vowel1 + Consonant + Vowel2   = Vowel1 (end of syllable)
    | (c1 == C) && c2 == C' && (c3 == C || c3 == G) = [c1, c2]                     --Consonant1 + "s" + Consonant2 = Consonant1 + "s"
getFirstCategorySyllable (c1:c2:xs)
    | (c1 == C || c1 == C') && (c2 == C || c2 == C') = [c1]                        --Consonant1 + Consonant2 = Consonant1
    | c1 == c2 && c1 == S = [c1]                                                   --Strong1 + Strong2       = Strong1
    | (c1 == C || c1 == C') && c2 == G = [c1]                                      --Consonant + ConsGroup   = Consonant
    | (c1 == W || c1 == S)  && c2 == G = [c1]                                      --Vowel + ConsGroup       = Vowel
getFirstCategorySyllable (x:xs) = x : getFirstCategorySyllable(xs)                 --If nothing can be divided yet, move along, bulding up list.

-------------- Stress ------------------------------

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

stressify :: [String] -> [String]
stressify [] = []
stressify [x] = [upper x]
stressify s
    | True `elem` map isAccented s = replaceAtIndex (fromJust (elemIndex True (map isAccented s))) (upper (head (filter isAccented s))) s
    | isAguda s = replaceAtIndex ((length s) - 1) (upper (last s)) s
    | isGrave s = replaceAtIndex ((length s) - 2) (upper (last (init s))) s

----------------------------------------------------

main = do
    putStrLn "Enter a Spanish word: "
    word <- getLine
    putStrLn (syllabify (lower word))
