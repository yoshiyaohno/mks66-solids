{-# LANGUAGE FlexibleContexts #-}
module Solids where

import Line
import Screen
import Transform

import Data.Maybe
import Control.Monad.State
import Data.Array.Unboxed
import qualified Data.List  as L

import Control.Exception

newtype Triangle a = Triangle (Vect a, Vect a, Vect a) deriving (Show, Eq)
data Pixel = Pixel
    { pgetX :: Int
    , pgetY :: Int
    , pgetZ :: Double
    } deriving (Eq, Show)

piStep :: Floating a => a
piStep = pi/11
--piStep = pi/30

lightPx :: Pixel -> ((Int, Int), Color)
lightPx (Pixel x y z) = ((x, y), (color 128 0 ((round z) `mod` 255)))

plotPxs :: (MonadState ZBuf m) => [Pixel] -> m [Pixel]
plotPxs = fmap catMaybes . mapM plotPx

plotPx :: MonadState ZBuf m => Pixel -> m (Maybe Pixel)
plotPx (Pixel x y z) = do
    zb <- get
    if z < zb!(x,y)
        then do modify $ modZB [((x,y), z)]
                return (Just $ Pixel x y z)
        else return Nothing

drawTriangle :: Color -> Triangle Double -> Screen -> Screen
drawTriangle c t =
    draw [((pgetX px, pgetY px), c) | px <- scanTriangle t]
    
lh :: (Eq a, Enum a, Fractional a) =>
    (Vect a -> a) -> Vect a -> Vect a -> [Vect a]
lh = lineHelper

-- returns the vertical line between two pixels
pixLiner :: Pixel -> Pixel -> [Pixel]
pixLiner p0 p1 = map vdToPix (lh getY (pixToVd p0) (pixToVd p1))
    -- ......
    -- I give up.

-- scans across a line of pixels (finding z values in between)
pixScan :: Pixel -> Pixel -> [Pixel]
pixScan (Pixel x0 y0 z0) (Pixel x1 y1 z1)
    | y0 /= y1  = error "mismatched y values in pixScan"
    | dx == 0   = [Pixel x0 y0 z0]
    | otherwise = [Pixel (x+x0) y0 ((fromIntegral x)*dz/(fromIntegral dx) + z0)
                  | x <- [0, (signum dx) .. dx]]
    where dx = x1 - x0; dz = z1 - z0

scanTriangle :: Triangle Double -> [Pixel]
scanTriangle (Triangle (a, b, c)) = let
    [bot, mid, top] = map vdToPix (L.sortOn getY [a, b, c])
    e1 = pixLiner bot top
    e2 = pixLiner mid top ++ tail (pixLiner bot mid)
    es = if 2*(pgetX mid) <= (pgetX top + pgetX bot)
            then zip e2 e1 else zip e1 e2
    in concatMap (uncurry pixScan) es

vdToPix :: Vect Double -> Pixel
vdToPix (Vect x y z q) = Pixel (round x) (round y) z
-- why
pixToVd :: Pixel -> Vect Double
pixToVd (Pixel x y z) = Vect (fromIntegral x) (fromIntegral y) z 1

toEdges :: Triangle a -> [Line a]
toEdges (Triangle (a, b, c)) = [Line a b, Line b c, Line a c]

drawTriangles :: Color -> [Triangle Double] -> Screen -> Screen
drawTriangles c = foldr (.) id . zipWith drawTriangle
    (cycle [red, grn, blu, wht]) . bfCull

--drawTriangles :: Color -> [Triangle Double] -> Screen -> Screen
--drawTriangles c =
--    foldr (.) id . map (drawLine c . fmap round)
--        . concat . map toEdges . bfCull

bfCull :: (Num a, Ord a) => [Triangle a] -> [Triangle a]
bfCull = filter ((>0) . getZ . normal)

normal :: (Num a) => Triangle a -> Vect a
normal (Triangle (a, b, c)) = crossProd ((-) <$> c <*> a) ((-) <$> b <*> a)

trTriangle :: (Num a) => Transform a -> Triangle a -> Triangle a
trTriangle t (Triangle (a, b, c)) = Triangle (pmult t a, pmult t b, pmult t c)

sphere :: (Floating a, Enum a) => a -> a -> a -> a -> [Triangle a]
sphere cx cy cz r = concat $ zipWith stitchLines (rotate 1 arcs) arcs
    where arcs = [[Vect (cx + r * cos thet) (cy + r * sin thet * cos phi)
                 (cz + r * sin thet * sin phi) 1
                 | thet <- [0, piStep .. pi]] | phi <- [0, 2*piStep .. 2*pi]]

torus :: (Floating a, Enum a) => a -> a -> a -> a -> a -> [Triangle a]
torus cx cy cz r0 r1 = concat $ zipWith stitchLines arcs (rotate 1 arcs)
    where arcs = [[Vect (cx + r0 * cos thet * cos phi + r1 * cos phi)
                  (cy + r0 * sin thet)
                  (cz - sin phi * (r0 * cos thet + r1)) 1
                  | thet <- [0, 2*piStep .. 2*pi]]
                  | phi <- [0, 2*piStep .. 2*pi]]

box :: (Floating a, Enum a) => a -> a -> a -> a -> a -> a -> [Triangle a]
box cx cy cz w h d = let
    [p000, p001, p010, p011, p100, p101, p110, p111] = 
        [Vect (cx + qx * w) (cy - qy * h) (cz - qz * d) 1
            | qx <- [0,1], qy <- [0,1], qz <- [0,1]]
                in stitch4 p000 p010 p110 p100
                   ++ stitch4 p100 p110 p111 p101
                   ++ stitch4 p111 p011 p001 p101
                   ++ stitch4 p110 p010 p011 p111
                   ++ stitch4 p011 p010 p000 p001
                   ++ stitch4 p000 p100 p101 p001
    -- y a h o o

stitchLines :: [Vect a] -> [Vect a] -> [Triangle a]
stitchLines [] _   = []
stitchLines _ []   = []
stitchLines _ [b0] = []
stitchLines [a0] _ = []
stitchLines (a0:a1:as) (b0:b1:bs) =
    (stitch4 a1 a0 b0 b1) ++ (stitchLines (a1:as) (b1:bs))

-- these four points go clockwise to face the normal into you
stitch4 :: Vect t -> Vect t -> Vect t -> Vect t -> [Triangle t]
stitch4 a b c d = [Triangle (a, b, c), Triangle (a, c, d)]

