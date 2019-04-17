module DrawMats where

import Data.Array.Unboxed
import Line
import Screen
import Solids
import Transform

type Args = [String]
data DrawMats =
     DrawMats { getScreen :: Screen
              , getTStack :: [Transform Double]
              , getZBuf   :: UArray (Int, Int) Double
--            , getEdges :: [Vect Double]
--            , getTriangles :: [Triangle Double]
              }

emptyDM :: DrawMats
emptyDM = DrawMats
    { getScreen = emptyScreen blk (500,500)
    , getTStack = [ident]
    , getZBuf   = array ((0,0),(500,500))
        [((x,y), 2**1024) | x <- [0..500], y <- [0..500]]
--  , getEdges = []
--  , getTriangles = []
    }

trTris :: DrawMats -> [Triangle Double] -> [Triangle Double]
trTris dm = map (trTriangle $ getTransform dm)

modScreen :: (Screen -> Screen) -> DrawMats -> DrawMats
modScreen f dm = dm { getScreen = f $ getScreen dm }

getTransform :: DrawMats -> Transform Double
getTransform = head . getTStack

popTransform :: DrawMats -> DrawMats
popTransform dm = dm { getTStack = tail $ getTStack dm }

pushTransform :: DrawMats -> DrawMats
pushTransform dm = dm { getTStack = (tf:tf:tfs) }
    where (tf:tfs) = getTStack dm

modTransform :: (Transform Double -> Transform Double) -> DrawMats -> DrawMats
modTransform f dm = dm { getTStack = ((f tf):tfs) }
    where (tf:tfs) = getTStack dm

