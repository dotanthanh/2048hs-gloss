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
                           , seed :: StdGen
                           , lost :: Bool }

instance Model GameState where
	render g
		| check = pictures [translate (negate 100) 150 . scale 0.2 0.2 . text $ "You lose keke", render $ gameBoard g]
		| otherwise = render $ gameBoard g
		where
			check = lost g

fps :: Int
fps = 60

window :: Display
window = InWindow "2048" (400, 400) (50, 50)

background :: Color
background = white

isLost :: GameState -> Bool
isLost GameState {gameBoard = b} = length gEmpty == 0
	where
		gEmpty = filter (\g -> value g == 0) b

spawnGrid :: GameState -> GameState
spawnGrid g
	| check == True = GameState { gameBoard = changeGrid (Grid n (x,y) False End 0) b
                                , seed = gen
                                , lost = lost g }
	| otherwise = spawnGrid GameState { gameBoard = b
                                      , seed = gen
                                      , lost = lost g }
	where
		check = isFree (x,y) b
		b = gameBoard g
		n
			| x + y > 1 = 2
			| otherwise = 4
		gen = snd . next . seed $ g
		(x:y:_) = randomRs (0,3) (seed g)

onMove :: Event -> GameState -> GameState
onMove (EventKey (SpecialKey k) Down _ _) g
	| k == KeyLeft = f L g
	| k == KeyRight = f R g
	| k == KeyUp = f U g
	| k == KeyDown = f D g
	| otherwise = g
	where
		f d g'
			| isLost g'' = GameState { gameBoard = gameBoard g''
                                     , seed = seed g'
                                     , lost = True }
			| b' == gameBoard g' = g'
			| otherwise = spawnGrid g''
			where
				g'' = GameState { gameBoard = b'
                                , seed = seed g'
								, lost = lost g' }
				b' = reduceBoard d (gameBoard g')
onMove _ g = g

onStep :: Float -> GameState -> GameState
onStep x g = GameState { gameBoard = map f $ gameBoard g
                       , seed = seed g
                       , lost = lost g }
	where
		f (Grid v p d b s)
			| s < 1 && d == False = Grid v p False End (s+x*3)
			| b == Inflate && s < 1.2 = Grid v p d b (s+x*3)
			| s >= 1.2 && b == Inflate = Grid v p d Shrink s
			| b == Shrink && s > 1 = Grid v p d b (s-x*3)
			| b == Shrink && s <= 1 = Grid v p d End s
			| otherwise = Grid v p True End 1
onStep _ g = g

main :: IO ()
main = do
	g <- getStdGen
	let initialGame = spawnGrid GameState { gameBoard = board
                                          , seed = g
                                          , lost = False }
	play window background fps initialGame render onMove onStep

testSpawn :: IO ()
testSpawn = do
	g <- getStdGen
	let game = GameState { gameBoard = board
                         , seed = g
                         , lost = False }
	let b = gameBoard $ spawnGrid game
	let check = 1 == length (filter (\(Grid n _ _ _ _) -> n /= 0) b)
	if check
		then putStrLn "True"
		else putStrLn "False"

