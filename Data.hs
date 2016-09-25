{-# LANGUAGE OverloadedStrings #-}

module Data where

import HGamer3D

import qualified Data.Text as T
import Data.Tree
import Data.Unique
import qualified Data.Map as M
import qualified Data.HMap as HM

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import System.Random

import Debug.Trace

-- state game is in (which screen, mode, ...)

data GameState = ProgramInitializing | InitScreen | PlayGame | Flying | FinalScore deriving (Eq, Ord, Show)

-- main data structure for game content - the game data tree
-- ---------------------------------------------------------

data NodeType = Canon | ReserveCanon | Boulder | Invader Int | Ship | Shot 
              | InvaderRow | CanonRow | BoulderRow | ShotRow
              | Pixel | PixelA | PixelB      -- the single pixel cubes, different variants for animation
              | Empty
              deriving (Eq, Show, Ord)

type NodeData = HM.HMap

type GameData = Tree (NodeType, NodeData)

type KPos = HM.HKey HM.T PixelPos
type KHits = HM.HKey HM.T HitInfo
type KDim = HM.HKey HM.T DimInfo
type KEnt = HM.HKey HM.T Entity
type KAnim = HM.HKey HM.T AnimInfo
type KUni = HM.HKey HM.T Unique

type Keys = (KEnt, KDim, KPos, KHits, KAnim, KUni)

genKeys :: IO Keys
genKeys = do     
    kent <- HM.createKey
    kdim <- HM.createKey
    kpos <- HM.createKey
    khits <- HM.createKey
    kanim <- HM.createKey
    kuni <- HM.createKey
    return (kent, kdim, kpos, khits, kanim, kuni)


-- data for all moving parts are kept in a flexible tree data structure
-- each node of this tree has a node type and node data

emptyData :: NodeData
emptyData = HM.empty

initData :: HM.HKey t a -> a -> NodeData
initData k v = HM.singleton k v

setData :: HM.HKey t a -> a -> NodeData -> NodeData
setData k v nd = HM.insert k v nd

(!) :: NodeData -> HM.HKey t a -> a
(!) nd k = case HM.lookup k nd of
    Just v -> v
    Nothing -> trace ("NodeData ! not there") undefined

isNodeType :: GameData -> NodeType -> Bool
isNodeType (Node (nt, _) _) nt' = nt == nt'

getChildren :: GameData -> [GameData]
getChildren (Node _ c) = c

addChildren :: GameData -> [GameData] -> GameData
addChildren (Node v c) c' = Node v (c ++ c')

removeChildren :: GameData -> GameData
removeChildren (Node v c) = Node v []

-- data concerning positions, damage and animations
-- ------------------------------------------------

type PixelPos = (Int, Int)   

pixelWidth :: Float
pixelWidth = 0.1

lineWidth :: Float
lineWidth = 0.02

data DimInfo = DimInfo {
    diWidth :: Int,
    diHeight :: Int,
    diCenterX :: Float,
    diCenterY :: Float,
    diOffX :: Float,
    diOffY :: Float }
    deriving (Eq, Show, Ord)

data HitInfo = HitInfo {
    hiHits :: Int,
    hiMaxHits :: Int
    }
    deriving (Eq, Show, Ord)

data AnimInfo = AnimInfo {
    aiCycles :: Int,
    aiStartCycle :: Int,
    aiSwapNow :: Bool,
    aiType :: NodeType -- PixelA or PixelB, current status
}

-- position, size data, all positions are given in pixel pos and converted to Vec3

diFromSize :: Int -> Int -> DimInfo
diFromSize width height = let
    cx = ((fromIntegral width) * pixelWidth + (fromIntegral (width - 1)) * lineWidth) / 2.0 
    cy = ((fromIntegral height) * pixelWidth + (fromIntegral (height - 1)) * lineWidth) / 2.0 
    ox = if (width `mod` 2) > 0 then (pixelWidth + lineWidth) * (-0.5) else 0.0     
    oy = if (height `mod` 2) > 0 then (pixelWidth + lineWidth) * (-0.5) else 0.0 
    in (DimInfo width height cx cy ox oy)

posFromPixelPos :: DimInfo -> PixelPos -> Vec3
posFromPixelPos di (x, y) = let
    delta = pixelWidth + lineWidth
    in Vec3 ((fromIntegral x) * delta + (diOffX di)) ((fromIntegral y) * delta + (diOffY di)) 0.0

relativePosFromPixelPos :: DimInfo -> PixelPos -> Vec3
relativePosFromPixelPos dim (x, y) = let 
    delta = pixelWidth + lineWidth
    in ((Vec3 ((fromIntegral x) * delta) ((fromIntegral y) * delta) 0.0) &- (Vec3 (diCenterX dim) (diCenterY dim) 0.0) )

-- damage, hits

hiNew :: Int -> HitInfo
hiNew maxHits = HitInfo 0 maxHits

takeHits :: HitInfo -> Int -> HitInfo
takeHits (HitInfo h mh) hits = (HitInfo (h + hits) mh)

damage :: HitInfo -> Float
damage (HitInfo h m) = if h >= m then 1.0 else (fromIntegral h) / (fromIntegral m)

-- animation info

aiNew :: IO AnimInfo   -- io for random number generator
aiNew = do
    cycles <- randomRIO (1, 5) :: IO Int
    startCycle <- randomRIO (1, cycles) :: IO Int
    b <- randomRIO (0, 1) :: IO Int
    let aiType = case b of
                    0 -> PixelA
                    1 -> PixelB
    return (AnimInfo cycles startCycle False aiType)

getCurrentAnimation :: AnimInfo -> Int -> AnimInfo   -- active, non-active node type
getCurrentAnimation anim cycle = if (cycle + aiStartCycle anim) `mod` (aiCycles anim) /= 0 
    then anim {aiSwapNow = False}
    else anim {aiType = case (aiType anim) of
                            PixelA -> PixelB
                            PixelB -> PixelA,
                        aiSwapNow = True
                        }

-- create moving entities
-- ----------------------

createMoveNode :: HG3D -> Keys -> NodeType -> PixelPos -> IO GameData
createMoveNode hg3d keys nodeType pos = do

    let (kent, kdim, kpos, khits, kanim, kuni) = keys

    let arts = artwork M.! nodeType
    let (ppA, ppB) = pixelPairs arts
    let (w, h) = dimArt arts
    let dim = diFromSize w h
    let hits = hiNew (hitData M.! nodeType)
    anim <- aiNew
    uni <- newUnique

    let delta = pixelWidth + lineWidth
    let mat = matArt arts

    eGeo <- newE hg3d [
        ctGraphicsElement #: (),
        ctScale #: Vec3 1.0 1.0 1.0,
        ctPosition #: posFromPixelPos dim pos,
        ctOrientation #: unitU
        ]

    let nd = setData kuni uni $ setData kanim anim $ setData kdim dim $ setData kent eGeo $ setData kpos pos $ initData khits hits

    eGeoId <- idE eGeo

    let createPE t (x, y) = do
            e <- newE hg3d [
                ctParent #: eGeoId,
                ctGeometry #: ShapeGeometry Cube,
                ctMaterial #: mat,
                ctScale #: Vec3 pixelWidth pixelWidth pixelWidth,
                ctPosition #: (if t == Pixel || t == PixelA then relativePosFromPixelPos dim (x, y) else (Vec3 (-1000) 0 0)), 
                ctOrientation #: unitU
                ]
            uni' <- newUnique
            return (e, (x, y), t, uni')
--            return (e, (x - ((diWidth dim) `div` 2), y - ((diHeight dim) `div` 2)), t)

    case ppB of
        Nothing -> do
            pes <- mapM (createPE Pixel) ppA
            return $ Node (nodeType, nd) (map (\(e, p, t, u) -> Node (t, (setData kuni u (setData kent e (initData kpos p)))) []) pes)

        Just ppB' -> do
            let ppBoth = filter (\p ->  p `elem` ppB') ppA
            pesBoth <- mapM (createPE Pixel) ppBoth

            let ppAOnly = filter (\p ->  not (p `elem` ppB')) ppA
            pesA <- mapM (createPE PixelA) ppAOnly

            let ppBOnly = filter (\p ->  not (p `elem` ppA)) ppB'
            pesB <- mapM (createPE PixelB) ppBOnly

            return $ Node (nodeType, nd) (map (\(e, p, t, u) -> Node (t, (setData kuni u (setData kent e (initData kpos p)))) []) (pesBoth ++ pesA ++ pesB))


gameDataFromBuildData :: HG3D -> Keys -> [BuildElement] -> IO GameData
gameDataFromBuildData hg3d keys bd = do
    let (kent, kdim, kpos, khits, kanim, kuni) = keys
    nodes <- mapM (\be -> case be of
                BEOne nt pix -> createMoveNode hg3d keys nt pix
                BERow nt pix@(x, y) space count -> do
                    let (nt', nt'') = case nt of
                            (Invader n) -> (InvaderRow, Invader n)
                            Boulder -> (BoulderRow, Boulder)
                            Canon -> (CanonRow, ReserveCanon)
                            Shot -> (ShotRow, Shot)
                    subnodes <- mapM (\pix' -> createMoveNode hg3d keys nt'' pix') [(x' + x, y) | x' <- [0, space .. ((count-1)*space)]] 
                    return (Node (nt', (HM.singleton kpos pix)) subnodes) 
        ) bd 
    return $ Node (Empty, HM.empty) nodes


moveNode :: Keys -> NodeData -> PixelPos -> IO NodeData
moveNode keys nodeData (x', y') = do

    let (kent, kdim, kpos, khits, kanim, kuni) = keys

    let (x, y) = nodeData ! kpos
    let nodeData' = setData kpos (x + x', y + y') nodeData
    let e = nodeData ! kent
    let di = nodeData ! kdim
    liftIO $ setC e ctPosition $ posFromPixelPos di (x + x', y + y')
    return nodeData'


-- data for flying
-- ---------------

data Speed = Speed Int

-- game level data
-- ---------------

data BuildElement = BEOne NodeType PixelPos
                    | BERow NodeType PixelPos Int Int -- space, count
                      deriving (Eq, Ord, Show)

type Artwork = ([T.Text], Maybe [T.Text], Material)

hitData :: M.Map NodeType Int
hitData = M.fromList [ (Shot, 1), (Canon, 0), (ReserveCanon, 0), (Boulder, 0), (Ship, 3), (Invader 1, 1), (Invader 2, 1), (Invader 3, 2) ]

-- get a list of (x, y) from Artwork
pixelPairs :: Artwork -> ([(Int, Int)], Maybe [(Int, Int)])
pixelPairs (a, b, _) = let
    ppFromLineArray lineArray = let
        lineIdx line = [ i | (i, c) <- zip [0..] (T.unpack line), c == 'x']
        in [ (i, j) | (j, l) <- zip [0..] (reverse lineArray), i <- lineIdx l]
    a' = ppFromLineArray a
    b' = fmap ppFromLineArray b
    in (a', b')

-- get width, height from Artwork
dimArt :: Artwork -> (Int, Int)
dimArt (lineArray, _, _) = let
    width = maximum (map (length . T.unpack) lineArray)
    height = length lineArray
    in (width, height)

matArt :: Artwork -> Material
matArt (_, _, mat) = mat

artwork :: M.Map NodeType Artwork
artwork = M.fromList [
  (Shot,
  (
    [
     "x",
     "x"
     ],
    Nothing,
    matYellow
  )),
  
  (Canon,
  (
    [
     "     x     ",
     "    xxx    ",
     "    xxx    ",
     " xxxxxxxxx ",
     "xxxxxxxxxxx",
     "xxxxxxxxxxx"
     ],
    Nothing,
     matLime
  )),
  
  (ReserveCanon,
  (
    [
     "     x     ",
     "    xxx    ",
     "    xxx    ",
     " xxxxxxxxx ",
     "xxxxxxxxxxx",
     "xxxxxxxxxxx"
     ],
    Nothing,
     matLime
  )),
  
  (Boulder,
  (
    [
     "    xxxxxxxxxxxx    ",
     "   xxxxxxxxxxxxxx   ",
     "  xxxxxxxxxxxxxxxx  ",
     " xxxxxxxxxxxxxxxxxx ",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxx        xxxxxx",
     "xxxxx          xxxxx",
     "xxxx            xxxx",
     "xxxx            xxxx"
     ],
    Nothing,
     matLime
  )),
  
  (Invader 1,
  (
    ["    xxxx    ",
     " xxxxxxxxxx ",
     "xxxxxxxxxxxx",
     "xxx  xx  xxx",
     "xxxxxxxxxxxx",
     "  xxx  xxx  ",
     " xx  xx  xx ",
     "  xx    xx  "],
    Just ["    xxxx    ",
     " xxxxxxxxxx ",
     "xxxxxxxxxxxx",
     "xxx  xx  xxx",
     "xxxxxxxxxxxx",
     "   xx  xx   ",
     "  xx xx xx  ",
     "xx        xx"],
     matGreen
  )),
  
  (Invader 2,
  (
    ["  x     x  ",
     "   x   x   ",
     "  xxxxxxx  ",
     " xx xxx xx ",
     "xxxxxxxxxxx",
     "x xxxxxxx x",
     "x x     x x",
     "   xx xx   "],
    Just ["  x     x  ",
     "x  x   x  x",
     "x xxxxxxx x",
     "xxx xxx xxx",
     "xxxxxxxxxxx",
     " xxxxxxxxx ",
     "  x     x  ",
     " x       x "],
     matRed
  )),
  
  (Invader 3,
  (
    ["   xx   ",
     "  xxxx  ",
     " xxxxxx ",
     "xx xx xx",
     "xxxxxxxx",
     " x xx x ",
     "x      x",
     " x    x "],
    Just ["   xx   ",
     "  xxxx  ",
     " xxxxxx ",
     "xx xx xx",
     "xxxxxxxx",
     "  x  x  ",
     " x xx x ",
     "x x  x x"],
     matMaroon
  )),
  
  (Ship,
  (
    ["     xxxxxx     ",
     "   xxxxxxxxxx   ",
     "  xxxxxxxxxxxx  ",
     " xx xx xx xx xx ",
     "xxxxxxxxxxxxxxxx",
     "  xxx  xx  xxx  ",
     "   x        x   "],
    Just ["     xxxxxx     ",
     "   xxxxxxxxxx   ",
     "  xxxxxxxxxxxx  ",
     " xx xx xx xx xx ",
     "xxxxxxxxxxxxxxxx",
     "  xxx  xx  xxx  ",
     "   x        x   "],
     matBlue
  ))
  
  
  ]

