import Pal (isPalindrome) 

--Io portion/interactive program--

main :: IO ()
main = 
    do
        word <- getLine
        print (verbose word)

verbose :: String -> String
verbose word =
    case (isPalindrome word) of
        Nothing -> "Please enter a word"
        Just True -> "yay palindrome!"
        Just False -> "sorry this is not palindrome!"
        
