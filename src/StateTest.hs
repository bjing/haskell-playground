module StateTest where

import Control.Monad.State
import Control.Monad
import System.Random
import Data.Functor.Identity
import Control.Applicative (liftA3, liftA)

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
  ((), newStack1) = push 3 stack
  (result, newStack2) = pop newStack1
  in pop newStack2

popS :: State Stack Int
popS = state $ \(x:xs) -> (x, xs)

pushS :: Int -> State Stack ()
pushS a = state $ \xs -> ((), a:xs)

stackManipS :: State Stack Int
stackManipS = do
  pushS 3
  popS
  popS

stackStuff :: State Stack ()
stackStuff = do
  a <- popS
  if a == 5
    then pushS 10
    else do
      pushS 3
      pushS 8

moreStack :: State Stack ()
moreStack = do
  a <- stackManipS
  -- if a == 100
  --   then stackStuff
  --   else return ()
  when(a == 100) stackStuff

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get
  if stackNow == [1,2,3]
    then put [8,3,1]
    else put [9,2,1]


randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)

threeCoins1 :: (Random a) => StateT StdGen Data.Functor.Identity.Identity [a]
threeCoins1 = replicateM 3 randomSt

threeCoins2 :: StateT StdGen Maybe (Bool, Bool, Bool)
threeCoins2 = undefined

threeCoins3 :: State StdGen (Bool, Bool, Bool)
threeCoins3 = (,,) <$> randomSt <*> randomSt <*> randomSt




-- newtype State s a = State { runState :: s -> (a,s) }

-- instance Monad (State s) where
--   return x = State $ \s -> (x,s)
--   state >>= f = State $ \s -> let (a, newState) = runState state $ s
--                                       (State g) = f a
--                                   in  g newState

