{-# LANGUAGE FlexibleContexts #-}

module Main(main) where

import System.Random
import Graphics.Gloss
import Grid
import Board
import Typeclass
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

data GameState = GameState {
	gameBoard :: Board
}

instance Model GameState where
	render (GameState {gameBoard=b}) = render b

fps :: Int
fps = 60

window :: Display
window = InWindow "2048" (300, 300) (50, 50)

background :: Color
background = white

onMove :: Event -> GameState -> GameState
onMove _ g = g

onStep :: Float -> GameState -> GameState
onStep _ g = g

main :: IO ()
main = do
	x <- randomRIO (0,3)
	y <- randomRIO (0,3)
	let	initialGame = GameState {gameBoard = changeGrid board (Grid 2 (x,y))}
	play window background fps initialGame render onMove onStep
