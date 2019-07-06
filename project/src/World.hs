module World (
    Cell(..)
  , Board
  , Point
  , BoardState
  , fillBoard
  , setCell
  , existWinner
  , existMove
  , getBoardPosition
) where

import Data.Matrix(Matrix, matrix, getElem, setElem)

data Cell = Black | White | Empty deriving (Show, Eq)

type Board = Matrix Cell
type Point = (Int, Int)

type BoardState = (Bool, Board)

fillBoard :: Board
fillBoard = matrix 3 3 (const Empty)

-- | here from [(1,1) -- (3,3)]
setCell :: Bool -> (Int, Int) -> Board -> Maybe Board
setCell b (x, y) board =
    let value = getElem x y board in
      let color = if b then White else Black in
        case value of
          Empty -> pure $ setElem color (x, y) board
          _ -> Nothing


existWinner :: Board -> Maybe (Cell, [Point])
existWinner board = innerChecker winnerPosition
  where
    innerChecker :: [Point] -> Maybe (Cell, [Point])
    innerChecker [] = Nothing
    innerChecker xs =
      let (fields, tail') = splitAt 3 xs in
        let black = foldr (func Black) True fields in
          let white = foldr (func White) True fields in
            if black
              then pure (Black, fields)
              else if white
                then pure (White, fields)
                else innerChecker tail'
    checkColor :: Cell -> Int -> Int -> Bool
    checkColor c x y = c == getElem x y board
    func :: Cell -> Point -> Bool -> Bool
    func color (x, y) b = b && checkColor color x y

existMove :: Board -> Bool
existMove   = foldr (\a b -> a == Empty || b) False

winnerPosition :: [Point]
winnerPosition =
  [(1, 1), (1, 2), (1, 3)] <>
  [(2, 1), (2, 2), (2, 3)] <>
  [(3, 1), (3, 2), (3, 3)] <>

  [(1, 1), (2, 1), (3, 1)] <>
  [(1, 2), (2, 2), (3, 2)] <>
  [(1, 3), (2, 3), (3, 3)] <>

  [(1, 1), (2, 2), (3, 3)] <>
  [(3, 1), (2, 2), (1, 3)]

getBoardPosition :: Int -> Int -> Int -> (Float, Float) -> (Int, Int)
getBoardPosition width height count (x, y) =
  let width' = (round x + offsetWidth) `div` stepWidth in
  let height' = (round y + offsetHeight) `div` stepHeight in
    (3 - height', width' + 1)
  where
    stepHeight = div height count
    stepWidth = div width count

    offsetHeight = div height 2
    offsetWidth = div width 2
