module Grid where

import Graphics.Gloss
import Typeclass

type Value = Int

type Position = (Int, Int)

data BounceState = Inflate | Shrink | End deriving (Eq, Show)

data Grid = Grid { value :: Value
                 , position :: Position
	             , doneInit :: Bool
	             , bounce :: BounceState
	             , size :: Float } deriving (Eq, Show)

-- left/right/up/down. Abbreviation is to avoid duplication with standard Left/Right
data Direction = L | R | U | D
	deriving (Eq, Show)

instance Model Grid where
	render Grid {value = 0} = blank
	render (Grid n (x,y) _ _ s)
		| x < 0 || x > 3 || y < 0 || y > 3 = blank
		| otherwise = scale s s . translate (x'/s) (y'/s) $ (pictures [container, txt])
			where
				x' = fromIntegral $ x*50
				y' = fromIntegral $ y*50
				container = rectangleWire 50 50
				txt
					| n == 2 || n == 4 || n == 8 = translate (negate 8) (negate 10) . scale 0.2 0.2 . text . show $ n
					| n == 16 || n == 32 || n == 64 = translate (negate 12) (negate 9) . scale 0.18 0.18 . text . show $ n
					| n == 128 || n == 256 || n == 512 = translate (negate 15) (negate 7) . scale 0.14 0.14 . text . show $ n
					| n == 1024 || n == 2048 || n == 4096 = translate (negate 18) (negate 5) . scale 0.12 0.12 . text . show $ n
					| otherwise = blank

combine :: Grid -> Grid -> (Grid, Int)
combine (Grid 0 p1 _ _ _) Grid {value = n} = (Grid n p1 True End 1, 1)
combine g Grid {value = 0} = (g, 1)
combine g1@(Grid n1 p1 _ _ _) Grid {value = n2}
	| n1 /= n2 = (g1, 2)
	| otherwise = (Grid (n1+n2) p1 True Inflate 1, 3)

move :: Direction -> Grid -> Grid
move L (Grid n (x,y) _ _ _) = Grid n (x-1, y) True End 1
move R (Grid n (x,y) _ _ _) =  Grid n (x+1, y) True End 1
move U (Grid n (x,y) _ _ _) = Grid n (x, y+1) True End 1
move D (Grid n (x,y) _ _ _) =  Grid n (x, y-1) True End 1

