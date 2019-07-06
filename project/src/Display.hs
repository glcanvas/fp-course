{-# LANGUAGE InstanceSigs #-}

module Display (
    buildGrid
  , drawActions
  , drawEndMessage
) where

import Data.Maybe (isJust)
import Graphics.Gloss.Interface.IO.Game (Picture(Pictures, Text, Translate, Color, Line)
  , Path
  , Color
  , green
  , blue
  , red
  , black)
import World(Board, Cell(..), existMove, existWinner)

import qualified Data.Matrix as M(matrix, (!))

buildGrid :: Int -> Int -> Int -> Picture
buildGrid width height ceilNum = Pictures $ map (Color black . Line) $ horizontalLines <> verticalLines
  where
    stepHeight = div height ceilNum
    stepWidth = div width ceilNum

    offsetHeight = div height 2
    offsetWidth = div width 2

    lines' :: (Int -> Path) -> Int -> Int -> [Path]
    lines' f size step = map (f . (* step)) . takeWhile (\x -> step * x < size) $ [0 ..]

    verticalLines :: [Path]
    verticalLines = lines' inner width stepWidth
      where
        inner :: Int -> Path
        inner x = [firstDot x, secondDot x]
        firstDot x = (fromIntegral (x - offsetWidth), fromIntegral $ -offsetHeight)
        secondDot x = (fromIntegral (x - offsetWidth), fromIntegral offsetHeight)

    horizontalLines :: [Path]
    horizontalLines = lines' inner height stepHeight
      where
        inner :: Int -> Path
        inner x = [firstDot x, secondDot x]
        firstDot x = (fromIntegral $  -offsetWidth, fromIntegral (x - offsetHeight))
        secondDot x = (fromIntegral offsetWidth, fromIntegral ( x - offsetHeight))

drawActions :: Board -> Int -> Int -> Int -> Picture
drawActions board width height count = foldr addPicture (Pictures []) (M.matrix 3 3 id)
  where
    addPicture :: (Int, Int) -> Picture -> Picture
    addPicture pos pic@(Pictures array) =
      let value = board M.! pos in
        case value of
          Black -> Pictures $ createLetter "X" red pos : array
          White -> Pictures $ createLetter "O" blue pos : array
          Empty -> pic
    addPicture _ _ = error "Unmached pattern"

    createLetter :: String -> Color -> (Int, Int) -> Picture
    createLetter s c (w, h) =
      let posW = stepHeight * (3 - w) - offsetHeight + (stepHeight `div` 2) in
        let posH = stepWidth * (h - 1) - offsetWidth + (stepWidth `div` 2) in
          Color c (Translate (fromIntegral posH) (fromIntegral posW) (Text s))

    stepHeight = div height count
    stepWidth = div width count

    offsetHeight = div height 2
    offsetWidth = div width 2

drawEndMessage :: Board -> Picture
drawEndMessage board =
  let winner = existWinner board in
    let move = existMove board in
      let predicate = isJust winner || not move in
        if predicate
          then Pictures $ pure $ maybe (draw "DRAW") (\(cell, _) -> toWinner cell) (existWinner board)
          else Pictures []
  where
    draw :: String -> Picture
    draw s = Translate 0 0 $ Color green $ Text s

    toWinner :: Cell -> Picture
    toWinner White = draw "Win O"
    toWinner _ = draw "Win X"
