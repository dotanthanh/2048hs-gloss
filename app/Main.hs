{-# LANGUAGE FlexibleContexts #-}

module Main(main) where

import System.Random
import Graphics.Gloss
import Grid
import Board
import Typeclass
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game hiding (shift)

data GameState = GameState { gameBoard :: Board
                           , seed :: StdGen }

instance Model GameState where
	render (GameState {gameBoard=b}) = render b

fps :: Int
fps = 120

window :: Display
window = InWindow "2048" (300, 300) (50, 50)

background :: Color
background = white

spawnGrid :: GameState -> GameState
spawnGrid g
	| check == True = GameState { gameBoard = changeGrid (Grid 2 (x,y)) b
                                , seed = gen }
	| otherwise = spawnGrid GameState { gameBoard = b
                                      , seed = gen }
	where
		check = isFree (x,y) b
		b = gameBoard g
		gen = snd . next . seed $ g
		(x:y:_) = randomRs (0,3) (seed g)

onMove :: Event -> GameState -> GameState
onMove (EventKey (SpecialKey k) _ _ _) g
	| k == KeyLeft = f L g
	| k == KeyRight = f R g
	| k == KeyUp = f U g
	| k == KeyDown = f D g
	| otherwise = g
	where
		f d g' = GameState { gameBoard = reduceBoard d $ gameBoard g'
                                     , seed = seed g' }
onMove _ g = g

onStep :: Float -> GameState -> GameState
onStep _ g = g

main :: IO ()
main = do
	g <- getStdGen
	let initialGame = GameState { gameBoard = board
                                , seed = g }
	play window background fps initialGame render onMove onStep

