{-# LANGUAGE BinaryLiterals #-}
module Screen where

import Data.Bits
import Data.Int
import Data.Array.Unboxed

type ZBuf   = UArray (Int, Int) Double
type Screen = UArray (Int, Int) Color
type Color  = Int32

draw :: [((Int,Int), Color)] -> Screen -> Screen 
draw l s = s // (filter (inRange (bounds s) . fst) l)

modZB :: [((Int, Int), Double)] -> ZBuf -> ZBuf
modZB l s = s // (filter (inRange (bounds s) . fst) l)

wht = color 255 255 255
blk = color 0 0 0
red = color 255 0 0
blu = color 0 0 255
grn = color 0 255 0

showC :: Color -> String
showC = unwords . map show . ([getR, getG, getB] <*>) . pure

{-# INLINE color #-}
color :: Color -> Color -> Color -> Color
color r g b = r `shiftL` 16 + g `shiftL` 8 + b

{-# INLINE getR #-}
getR :: Color -> Color
getR = (`shiftR` 16)

{-# INLINE getG #-}
getG :: Color -> Color
getG = (.&. 0b11111111) . (`shiftR` 8)

{-# INLINE getB #-}
getB :: Color -> Color
getB = (.&. 0b11111111)

emptyZB :: (Int, Int) -> ZBuf
emptyZB (w,h) = array ((0,0),(w,h))
        [((x,y), 2**1024) | x <- [0..w], y <- [0..h]]

emptyScreen :: Color -> (Int, Int) -> Screen
emptyScreen c (w,h) =
    array ((0,0), (w,h)) [((x,y), c) | x <- [0..w], y <- [0..h]]

-- takes a screen and puts in ppm format
printPixels :: Screen -> String
printPixels scrn =
    ppmHeader (w1-w0, h1-h0)
    ++ (unlines . (map unwords) $
        [[showC $ scrn!(x, y) | x <- [w0..w1-1]] | y <- reverse [h0..h1-1]])
            where ((w0,h0), (w1,h1)) = bounds scrn
       
 
ppmHeader :: (Int, Int) -> String
ppmHeader (w, h) = "P3 " ++ show (w) ++ " " ++ show (h) ++ " 255\n"

