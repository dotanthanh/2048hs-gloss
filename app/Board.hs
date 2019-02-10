{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Board where

import System.Random
import Data.List (transpose, find)
import Data.Maybe (fromMaybe)
import Graphics.Gloss
import Grid
import Typeclass
import UIConfig

type Board = [Grid]

boardFrame :: Picture
boardFrame = pictures [fullboard, slots]
	where
		fullboard = color boardBackground $ rectangleSolid boardSize boardSize
		slots = translate fitGridToBoard fitGridToBoard . pictures $ map f [0,1..15]
		f n = translate (x*size) (y*size) $ color slotBackground (rectangleSolid gridSize gridSize)
			where
				size = gridSize + dividerSize
				x = fromIntegral $ mod n 4
				y = fromIntegral $ div n 4

instance Model Board where
	render b = pictures [boardFrame, grids]
		where
			grids = translate fitGridToBoard fitGridToBoard (pictures $ map render b)

board :: Board
board = map f [0,1..15]
	where
		f n = Grid 0 (mod n 4, div n 4) True End 1

-- check if the grid is occupied already
isFree :: Position -> Board -> Bool
isFree p b = g /= Nothing
	where
		g = find f b
		f = \(Grid n p' _ _ _) -> p == p' && n == 0

-- change the value of a grid
changeGrid :: Grid -> Board -> Board
changeGrid _ [] = []
changeGrid g@(Grid {position = (x,y)}) (b@(Grid {position = (gx, gy)}):bs)
	| gx == x && gy == y = [g] ++ bs
	| otherwise = [b] ++ changeGrid g bs

-- reduce a column/row of numbers
reduce :: Direction -> [Grid] ->  [Grid]
reduce _ [] = []
reduce _ [g]  = [g]
reduce d b@(g1:g2:gs)
	| snd result == 1 = reduce d ([g'] ++ shift gs) ++ [Grid 0 p True End 1]
	| snd result == 2 = [g1] ++ reduce d (g2:gs)
	| otherwise = [g'] ++ reduce d (shift gs) ++ [Grid 0 p True End 1]
	where
		shift = map (move d)
		Grid {position = p} = last b
		g' = fst result
		result = combine g1 g2

-- get Grids from Board as columns/rows
getGrids :: Direction -> Board -> [[Grid]]
getGrids d b
	| d == L = leftGrids
	| d == R = map reverse leftGrids
	| d == U = map reverse . transpose $ leftGrids
	| otherwise = transpose leftGrids
	where
		leftGrids = [ [g | g <- b, getY g == 0]
					, [g | g <- b, getY g == 1]
					, [g | g <- b, getY g == 2]
					, [g | g <- b, getY g == 3] ]
		getY = snd . position

fromGrids :: Direction -> [[Grid]] -> Board
fromGrids d
	| d == L = foldl1 (++)
	| d == R = foldl1 (++) . map reverse
	| d == U = foldl1 (++) . transpose . map reverse
	| otherwise = foldl1 (++) . transpose

reduceBoard :: Direction -> Board -> Board
reduceBoard d = fromGrids d . map (reduce d) . getGrids d

