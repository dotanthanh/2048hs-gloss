{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Board where

import System.Random
import Data.List (transpose, find)
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

-- check if the grid is occupied already
isFree :: Position -> Board -> Bool
isFree p b = g /= Nothing
	where
		g = find f b
		f = \(Grid n p') -> p == p' && n == 0

-- change the value of a grid
changeGrid :: Grid -> Board -> Board
changeGrid _ [] = []
changeGrid g@(Grid _ (x, y)) (b@(Grid _ (gx, gy)):bs)
	| gx == x && gy == y = [g] ++ bs
	| otherwise = [b] ++ changeGrid g bs

-- reduce a column/row of numbers
reduce :: Direction -> [Grid] ->  [Grid]
reduce _ [] = []
reduce _ [g]  = [g]
reduce d b@(g1:g2:gs)
	| snd result == 1 = reduce d ([g'] ++ shift gs) ++ [Grid 0 p]
	| snd result == 2 = [g1] ++ reduce d (g2:gs)
	| otherwise = [g'] ++ reduce d (shift gs) ++ [Grid 0 p]
	where
		shift = map (move d)
		Grid _ p = last b
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

testReduce :: Bool
testReduce = reduce R [ Grid 2 (3,0)
                      , Grid 2 (2,0)
                      , Grid 2 (1,0)
                      , Grid 2 (0,0) ] == [ Grid 4 (3,0)
                                          , Grid 4 (2,0)
                                          , Grid 0 (1,0)
                                          , Grid 0 (0,0) ]

testGetGrids :: Bool
testGetGrids = getGrids U board == [ [Grid 0 (0,3), Grid 0 (0,2), Grid 0 (0,1), Grid 0 (0,0)]
                                   , [Grid 0 (1,3), Grid 0 (1,2), Grid 0 (1,1), Grid 0 (1,0)]
                                   , [Grid 0 (2,3), Grid 0 (2,2), Grid 0 (2,1), Grid 0 (2,0)]
                                   , [Grid 0 (3,3), Grid 0 (3,2), Grid 0 (3,1), Grid 0 (3,0)] ]

testFromGrids :: Bool
testFromGrids = board == fromGrids U [ [Grid 0 (0,3), Grid 0 (0,2), Grid 0 (0,1), Grid 0 (0,0)]
                                     , [Grid 0 (1,3), Grid 0 (1,2), Grid 0 (1,1), Grid 0 (1,0)]
                                     , [Grid 0 (2,3), Grid 0 (2,2), Grid 0 (2,1), Grid 0 (2,0)]
                                     , [Grid 0 (3,3), Grid 0 (3,2), Grid 0 (3,1), Grid 0 (3,0)] ]

testIsFree :: Bool
testIsFree = False == isFree (1,2) [ Grid 0 (0,3), Grid 0 (0,2), Grid 0 (0,1), Grid 0 (0,0)
                                   , Grid 0 (1,3), Grid 2 (1,2), Grid 0 (1,1), Grid 0 (1,0)
                                   , Grid 0 (2,3), Grid 0 (2,2), Grid 0 (2,1), Grid 0 (2,0)
                                   , Grid 0 (3,3), Grid 0 (3,2), Grid 0 (3,1), Grid 0 (3,0) ]

