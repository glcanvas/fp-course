module Block2 (
  eval
  , ArithmeticError(PowNegate, DivByZero)
  , Expression(Const, Add, Sub, Mull, Div, Pow)
  , moving
) where

import Queue
import Control.Monad.State

-- | Data type for expression
data Expression
  = Const Int -- constructor of constant
  | Add Expression Expression -- constructor of recursion addition
  | Sub Expression Expression -- constructor of recursion substract
  | Mull Expression Expression -- constructor of recursion multiplication
  | Div Expression Expression -- constructor of recursion division
  | Pow Expression Expression -- constructor of recursion power
  deriving Show

-- | Data type for Arithmetic errors such as division by zero and power in negate
data ArithmeticError
  = PowNegate -- constructor of error power negate
  | DivByZero -- constructor of division by zero
  deriving (Show, Eq)

-- | Function that calculate "Expression" and return Either error or result evaluation
eval :: Expression -> Either ArithmeticError Int
eval (Const x) = Right x
eval (Add l r) =
  let leftEval = eval l
   in let rightEval = eval r
       in rightEval >>= hmm leftEval
  where
    hmm :: Either ArithmeticError Int -> Int -> Either ArithmeticError Int
    hmm z x = fmap (+ x) z

eval (Div l r) =
  let leftEval = eval l in
    let rightEval = eval r in
      rightEval >>= hmm leftEval
  where
    hmm :: Either ArithmeticError Int -> Int -> Either ArithmeticError Int
    hmm z x =
      if x == 0
        then Left DivByZero
        else fmap (`div` x) z

eval (Mull l r) =
  let leftEval = eval l
   in let rightEval = eval r
       in rightEval >>= hmm leftEval
  where
    hmm :: Either ArithmeticError Int -> Int -> Either ArithmeticError Int
    hmm z x = fmap (* x) z

eval (Sub l r) =
  let leftEval = eval l
   in let rightEval = eval r
       in rightEval >>= hmm leftEval
  where
    hmm :: Either ArithmeticError Int -> Int -> Either ArithmeticError Int
    hmm z x = fmap (\y -> y - x) z

eval (Pow l r) =
  let leftEval = eval l
   in let rightEval = eval r
       in rightEval >>= hmm leftEval
  where
    hmm :: Either ArithmeticError Int -> Int -> Either ArithmeticError Int
    hmm z x =
      if x < 0
        then Left PowNegate
        else fmap (  ^ x) z

-- | Define new type for "State" monad for calculate moving function
-- which is value of "State"
type MovingValue = [Float]

-- | Define new type for state monad for calculate moving function
-- which is state of "State"
type MovingState = (Queue Int, Int, [Float], [Int])

-- | Function that implement [https://en.wikipedia.org/wiki/Moving_average]
--
-- >>> moving 2 [1, 5, 3, 8, 7, 9, 6]
-- [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
--
moving :: Int -> [Int] -> [Float]
moving n array =
  let resMonad = innerCall (queueEmpty, 0, [], array)  (state innerState)  in
     let (x, y) = runState resMonad  (queueEmpty, 0, [], array) in
      x
  where
    innerState :: MovingState -> (MovingValue, MovingState)
    innerState (q, s, floatArray, []) = (floatArray, (q, s, floatArray, []))
    innerState (q, s, floatArray, x:xs) =
      if queueSize q < n
      then
        let newQueue = queuePush x q in
          let newSum = s + x in
            let newAvgItem = toFloat newSum / toFloat (queueSize newQueue) in
              let newAvg = newAvgItem : floatArray in
                 (newAvg, (newQueue, newSum, newAvg, xs))
      else
        let Just (newQueue, oldElement) = queuePop $ queuePush x q in
          let newSum = s + x - oldElement in
            let newAvgItem = toFloat newSum / toFloat n in
              let newAvg = newAvgItem : floatArray in
                (newAvg, (newQueue, newSum, newAvg, xs))
    innerCall :: MovingState -> State MovingState MovingValue -> State MovingState MovingValue
    innerCall (q, s, f, []) _ = return $ reverse f
    innerCall ms st =
      let (mv', ms') = runState st ms in
       innerCall ms' st
    toFloat :: Int -> Float
    toFloat x = fromIntegral x :: Float