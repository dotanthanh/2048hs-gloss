{-# LANGUAGE FlexibleContexts #-}

module Main(main) where

import System.Random
import Graphics.Gloss
import Grid
import Board
import Typeclass
import Graphics.Gloss.Data.ViewPort

window :: Display
window = InWindow "2048" (600, 600) (50, 50)

background :: Color
background = white

main :: IO ()
main = do
	x <- randomRIO (0,3)
	y <- randomRIO (0,3)
	display window background $ render $ changeGrid board (Grid 2 (x,y))

