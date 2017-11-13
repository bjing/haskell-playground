module GetPassphrase where

import Data.Char

getPassphrase :: IO (Maybe String)
getPassphrase = do
    s <- getLine
    if isValid s then return $ Just s else return Nothing

-- The validation test could be anything we want it to be.
isValid :: String -> Bool
isValid s = length s >= 8 && any isAlpha s && any isNumber s && any isPunctuation s

askPassphrase :: IO ()
askPassphrase = do
    putStrLn "Insert your new passphrase: "
    maybe_value <- getPassphrase
    case maybe_value of
        Just value -> putStrLn "Storing in database..." -- do stuff
        Nothing -> putStrLn "Passphrase invalid."

main :: IO ()
main = do
    s <- getPassphrase
    putStrLn s
