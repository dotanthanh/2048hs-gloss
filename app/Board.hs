{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Board where

import System.Random
import Data.Maybe (fromMaybe)
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

-- change the value of a grid
changeGrid :: Grid -> Board -> Board
changeGrid _ [] = []
changeGrid g@(Grid _ (x, y)) (b@(Grid _ (gx, gy)):bs)
	| gx == x && gy == y = [g] ++ bs
	| otherwise = [b] ++ changeGrid g bs

-- reduce a column/row of numbers
reduce :: Direction -> [Grid] ->  [Grid]
reduce _ [g]  = [g]
reduce d (g:gs) = [fst result] ++ f gs
	where
		f
			| snd result == Nothing = shift d . reduce d
			| otherwise = ([fromMaybe g $ snd result] ++) . reduce d
		result =  combine g $ head gs

-- shift a column/row of numbers
shift :: Direction -> [Grid] -> [Grid]
shift _ [] = []
shift d gs = map (move d) $ tail gs ++ [Grid 0 p]
	where
		Grid _ p = last gs

