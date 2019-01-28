{-# LANGUAGE FlexibleContexts #-}

module Main(main) where

import System.Random
import Graphics.Gloss
import Grid
import Board
import Typeclass
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game hiding (shift)

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
onMove (EventKey (SpecialKey KeyLeft) _ _ _) g = GameState {gameBoard = reduceBoard L $ gameBoard g}
onMove (EventKey (SpecialKey KeyRight) _ _ _) g = GameState {gameBoard = reduceBoard R $ gameBoard g}
onMove (EventKey (SpecialKey KeyUp) _ _ _) g = GameState {gameBoard = reduceBoard U $ gameBoard g}
onMove (EventKey (SpecialKey KeyDown) _ _ _) g = GameState {gameBoard = reduceBoard D $ gameBoard g}
onMove _ g = g

onStep :: Float -> GameState -> GameState
onStep _ g = g

main :: IO ()
main = do
	x <- randomRIO (0,3)
	y <- randomRIO (0,3)
	let	initialGame = GameState {gameBoard = changeGrid (Grid 2 (x,y)) $ board} 
	play window background fps initialGame render onMove onStep
