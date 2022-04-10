module Pal where
import Data.Char

isPalindrome :: String -> Maybe Bool
isPalindrome string = isOwnReverseMaybe (rejectEmpty (normalize string))

rejectEmpty :: String -> Maybe String
rejectEmpty word = 
    case word of
        [] -> Nothing
        _ -> Just word

normalize :: String -> String
normalize string = 
    filter notPunctuation (filter notSpace (alllowerCase string))

isOwnReverseMaybe :: Maybe String -> Maybe Bool
isOwnReverseMaybe maybeString = 
    case maybeString of
        Nothing -> Nothing
        Just string -> Just (isOwnReverse string)

isOwnReverse :: String -> Bool
isOwnReverse word = 
    word == reverse word

isPalindromePhrase :: String -> Bool
isPalindromePhrase phrase = 
    isOwnReverse (filter notSpace phrase)

isPalindromeIgnoringCase :: String -> Bool
isPalindromeIgnoringCase word = isOwnReverse (alllowerCase word)

notSpace :: Char -> Bool
notSpace x = not (x == ' ')    

notPunctuation :: Char -> Bool
notPunctuation x = not (isPunctuation x)

alllowerCase :: String -> String
alllowerCase word = map toLower (word)