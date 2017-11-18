{-# LANGUAGE OverloadedStrings #-}
module StateTTest where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Text.Read as TR

--
-- layer an infinite list of uniques over the IO monad
--

stateTTest :: IO ()
stateTTest = runStateT code [1..] >> return ()

code :: StateT [Integer] IO ()
code = do
    x <- pop
    io $ print x
    y <- pop
    io $ print y
    return ()

--
-- pop the next unique off the stack
--
pop :: StateT [Integer] IO Integer
pop = do
    (x:xs) <- get
    put xs
    return x

io :: IO a -> StateT [Integer] IO a
io = liftIO

-- runTest :: IO ()
-- runTest = runStateT

run :: StateT [Integer] IO ()
run = do
    i <- liftIO readLn
    cs1 <- get
    liftIO $ print cs1
    add i
    cs2 <- get
    liftIO $ print cs2
    return ()

run1 :: StateT [Integer] IO ()
run1 = do
    cs1 <- get
    liftIO $ print cs1

    i <- liftIO getLine
    case TR.readMaybe i of
        Just x -> add x
        _ -> return ()

    cs2 <- get
    liftIO $ print cs2

add :: Integer -> StateT [Integer] IO ()
add i = do
    xs <- get
    put (i:xs)
    return ()

readUserName :: MaybeT IO String
readUserName = MaybeT $ do
    str <- getLine
    if length str > 5
    then return $ Just str
    else return Nothing

maybeTest :: IO ()
maybeTest = do
    maybeUser <- runMaybeT readUserName
    case maybeUser of
        Nothing -> putStrLn "Couldn't login!"
        Just u -> putStrLn u



type Env = (Maybe String, Maybe String, Maybe String)
readUserName2 :: MaybeT (ReaderT Env IO) String
readUserName2 = MaybeT $ do
    (maybeOldUser, _, _) <- ask
    case maybeOldUser of
        Just str -> return str
        Nothing -> do
            -- lift allows normal IO functions from inside ReaderT Env IO!
            input <- lift getLine
            if length input > 5
            then return (Just input)
            else return Nothing
