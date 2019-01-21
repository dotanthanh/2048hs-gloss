{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Board where

import System.Random
import Graphics.Gloss
import Grid
import Typeclass

type Board = [Grid]

instance Model Board where
	render b = pictures [rectangleWire 200 200, container]
		where
			container = translate (negate 75) (negate 75) board
			board = pictures $ map render b

board :: Board
board = map f [0,1..15]
	where
		f n = Grid 0 (mod n 4, div n 4)

--addNewGrid :: RandomGen g => Board -> g -> Board
--addNewGrid [] _ = []
--addNewGrid b g =
--	where	be = filter f b
--			f = \(Grid n _) -> n==0
--			p = (head xy, tail xy)
--			xy = take 2 . randomRs (0,3) . snd $ g'
--			g' = next g

changeGrid :: Board -> Grid -> Board
changeGrid [] _ = []
changeGrid (b@(Grid _ (gx, gy)):bs) g@(Grid _ (x, y))
	| gx == x && gy == y = [g] ++ bs
	| otherwise = [b] ++ changeGrid bs g

