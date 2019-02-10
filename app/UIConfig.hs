module UIConfig where

import Graphics.Gloss.Data.Color

gridSize :: Float 
gridSize = 50

dividerSize :: Float
dividerSize = 10

boardSize :: Float
boardSize = gridSize * 4 + dividerSize * 5

boardSlot :: Float
boardSlot = gridSize + dividerSize

txtScale1Digit :: Float
txtScale1Digit = 0.2

txtScale2Digit :: Float
txtScale2Digit = 0.18

txtScale3Digit :: Float
txtScale3Digit = 0.14

txtScale4Digit :: Float
txtScale4Digit = 0.12

txtPos1Digit :: (Float, Float)
txtPos1Digit = (negate 8, negate 10)

txtPos2Digit :: (Float, Float)
txtPos2Digit = (negate 12, negate 9)

txtPos3Digit :: (Float, Float)
txtPos3Digit = (negate 15, negate 7)

txtPos4Digit :: (Float, Float)
txtPos4Digit = (negate 18,negate 5)

fitGridToBoard :: Float
fitGridToBoard = negate (dividerSize*1.5 + gridSize*1.5)

boardBackground :: Color
boardBackground = greyN 0.8

slotBackground :: Color
slotBackground = greyN 0.95

txtWhite :: Color
txtWhite = white

txtBlack :: Color
txtBlack = black

gridColors :: [Color]
gridColors = [ makeColorI 238 228 218 255   -- #eee4da
             , makeColorI 237 224 200 255   -- #ede0c8
             , makeColorI 242 177 121 255   -- #f2b179
             , makeColorI 245 149 99  255   -- #f59563
             , makeColorI 246 124 95  255   -- #f67c5f
             , makeColorI 246 94  59  255   -- #f65e3b
             , makeColorI 237 207 114 255   -- #edcf72
             , makeColorI 237 204 97  255   -- #edcc61
             , makeColorI 237 200 80  255   -- #edc850
             , makeColorI 237 197 63  255   -- #edc53f
             , makeColorI 237 194 46  255   -- #edc22e
             , makeColorI 255 255 255 255 ] -- #ffffff

