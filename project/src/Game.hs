{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game (
  executeGame
) where

import Data.Maybe (isJust, fromMaybe)
import Graphics.Gloss.Interface.Environment(getScreenSize)
import Graphics.Gloss.Interface.IO.Game(Picture(Pictures)
  , Event(EventKey)
  , Key(MouseButton, SpecialKey)
  , SpecialKey(KeyEsc)
  , MouseButton(LeftButton)
  , KeyState(Up)
  , Display(FullScreen)
  , makeColor
  , playIO)
import System.Exit (exitSuccess)
import Display (buildGrid, drawActions, drawEndMessage)
import World (BoardState, fillBoard, existWinner, existMove, setCell, getBoardPosition)
import Utils (readProperties)

import qualified Data.Map as Map

-- | Fist argument is path to file *.properties
executeGame :: Maybe String -> IO ()
executeGame maybePath =
  case maybePath of
    Nothing -> exec False
    Just path -> do
      properties <- readProperties path
      let first = fromMaybe "x" $ Map.lookup "first" properties
      if first == "x"
        then exec False
        else exec True
  where
    exec :: Bool -> IO ()
    exec b = playIO FullScreen (makeColor 255 255 255 0) 1 (b, fillBoard) drawBoardState handleAction (const pure)

drawBoardState :: BoardState ->  IO Picture
drawBoardState (_, board) = do
  (x,y) <- getScreenSize

  let Pictures array = buildGrid x y 3
  let Pictures array' = drawActions board x y 3
  let Pictures array'' = drawEndMessage board
  if not $ null array''
    then pure $ Pictures array''
    else pure $ Pictures $ array' <> array

handleAction :: Event -> BoardState -> IO BoardState
handleAction (EventKey (SpecialKey KeyEsc) _ _ _) _ = exitSuccess
handleAction (EventKey (MouseButton LeftButton) Up _ (x, y)) state = do
  (w, h) <- getScreenSize
  let position = getBoardPosition w h 3 (x, y)
  pure $ changeState state position
handleAction _ s = pure s

changeState :: BoardState -> (Int, Int) -> BoardState
changeState state'@(way, board') position =
  if resetBoard
    then (way, fillBoard)
    else processAction state' position
  where
    resetBoard :: Bool
    resetBoard = isJust (existWinner board') || not (existMove board')
    processAction :: BoardState -> (Int, Int) -> BoardState
    processAction state@(player, board) pos = maybe state (not player, ) (setCell player pos board)
