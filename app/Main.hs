{-# LANGUAGE FlexibleContexts #-}

module Main(main) where

import Graphics.Gloss
import Grid

window :: Display
window = InWindow "2048" (600, 600) (50, 50)

background :: Color
background = white

board = pictures grids
	where
		grids = map getPicture $ map f [0,1..15]
		f n = Grid 2 (mod n 4) (div n 4)

main :: IO ()
main = display window background $ board
