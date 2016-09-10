{-# LANGUAGE OverloadedStrings #-}

module Move where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
import System.Exit
import System.Random

import qualified Data.Map as M
import qualified Data.HMap as HM
import qualified Data.Text as T
import Data.Tree
import Data.Maybe
import qualified Data.Data as D
import qualified Data.Traversable as Tr
import qualified Data.Foldable as Fd
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Debug.Trace

import Data
import Actor


-- MOVEMENT ACTOR
-- --------------

type MoaR = (HG3D, Actor, Actor, KEnt, KDim, KPos, KHits, KAnim)
type MoaS = (Int, PixelPos, PixelPos, GameData)

mapAccumLM f a xs = runStateT (Tr.mapM (StateT . f) xs) a

movementActorF :: Message -> ReaderStateIO MoaR MoaS ()
movementActorF m = do

    (hg3d, screenA, musicA, kent, kdim, kpos, khits, kanim) <- lift ask
    (c, canonPos, canonMove, gameData) <- get

    case m of

        BuildLevel buildData1 -> do
            createLevel buildData1
            liftIO  $ sendMsg screenA BuildDone
            return ()

        MoveLeft -> do
            let (x, y) = canonMove
            let (px, py) = canonPos
            if (px > -65)
                then put (c, (px - 1, py), (x - 1, y), gameData)
                else return ()

        MoveRight ->  do
            let (x, y) = canonMove
            let (px, py) = canonPos
            if (px < 100)
                then put (c, (px + 1, py), (x + 1, y), gameData)
                else return ()

        Shoot -> do
            bulletAvailable <- shoot
            if bulletAvailable 
                then liftIO $ sendMsg musicA PlayShot
                else liftIO $ sendMsg musicA PlayNoShot
            return ()

        MovementCycle -> do

            let moves = c `div` nWait + (movesRightLeft `div` 2) -- start in the middle 
            let bMove = (c `mod` nWait == 0) && ((c `div` nWait) < (( 2 * movesRightLeft + 2 ) * movesDown) ) 
            let move = moves `mod` (2 * movesRightLeft + 2)

            -- general movement
            gameData' <- mapM (\(nodeType, nodeData) -> do
                nodeData' <- case nodeType of
                    -- invaders stepping
                    (Invader _) -> do invaderMove nodeData bMove move
                    -- shots moving
                    Shot -> do
                        nd <- moveNode nodeData (0, 2)
                        let (x, y) = nd ! kpos
                        if y > 100 
                            then return (deactivateShot nd kpos)
                            else return nd
                    -- canon move
                    Canon -> moveNode nodeData canonMove
                    -- no move
                    _ -> return nodeData
                return (nodeType, nodeData')
                ) gameData

            -- pixel animation
            (gameData'', _) <- if bMove 
                then
                    liftIO (mapAccumLM (\(nodeType, nodeData) (nt, nd) -> case nodeType of
                        (Invader _) -> do
                            let ai = nodeData ! kanim
                            let ai' = getCurrentAnimation ai moves
                            let nodeData' = setData kanim ai' nodeData
                            return ((nodeType, nodeData'), (nodeType, nodeData')) -- set acc to nodeData of Invader

                        PixelA -> do
                            let ai = nd ! kanim         -- accu, this is the previous Invader node info!
                            if aiSwapNow ai
                                then do
                                    let (x, y) = nodeData ! kpos
                                    case aiType ai of
                                        PixelA -> setC (nodeData ! kent) ctPosition $ relativePosFromPixelPos (nd ! kdim) (x, y)
                                        PixelB -> setC (nodeData ! kent) ctPosition (Vec3 (-1000.0) 0 0)
                                        _ -> return ()
                                else return ()
                            return ((nodeType, nodeData), (nt, nd))

                        PixelB ->  do
                            let ai = nd ! kanim         -- accu, this is the previous Invader node info!
                            if aiSwapNow ai
                                then do
                                    let (x, y) = nodeData ! kpos
                                    case aiType ai of
                                        PixelB -> setC (nodeData ! kent) ctPosition $ relativePosFromPixelPos (nd ! kdim) (x, y)
                                        PixelA -> setC (nodeData ! kent) ctPosition (Vec3 (-1000.0) 0 0)
                                        _ -> return ()
                                else return ()
                            return ((nodeType, nodeData), (nt, nd))

                        _ -> return ((nodeType, nodeData), (nt, nd)) -- noch changes

                    ) (undefined, undefined) gameData')
                else return (gameData', undefined)

            -- hit detection


            put (c + 1, canonPos, (0, 0), gameData'') 

        _ -> return ()



-- create a node with subnodes for moveable item and cubes below

createMoveNode :: NodeType -> PixelPos -> ReaderStateIO MoaR MoaS GameData
createMoveNode nodeType pos = do

    (hg3d, screenA, musicA, kent, kdim, kpos, khits, kanim) <- lift ask

    let arts = artwork M.! nodeType
    let (ppA, ppB) = pixelPairs arts
    let (w, h) = dimArt arts
    let dim = diFromSize w h
    let hits = hiNew (hitData M.! nodeType)
    anim <- liftIO $ aiNew

    let delta = pixelWidth + lineWidth
    let mat = matArt arts

    eGeo <- liftIO $ newE hg3d [
        ctGraphicsElement #: (),
        ctScale #: Vec3 1.0 1.0 1.0,
        ctPosition #: posFromPixelPos dim pos,
        ctOrientation #: unitU
        ]

    let nd = setData kanim anim $ setData kdim dim $ setData kent eGeo $ setData kpos pos $ initData khits hits

    eGeoId <- liftIO $ idE eGeo

    let createPE t (x, y) = do
            e <- newE hg3d [
                ctParent #: eGeoId,
                ctGeometry #: ShapeGeometry Cube,
                ctMaterial #: mat,
                ctScale #: Vec3 pixelWidth pixelWidth pixelWidth,
                ctPosition #: (if t == Pixel || t == PixelA then relativePosFromPixelPos dim (x, y) else (Vec3 (-1000) 0 0)), 
                ctOrientation #: unitU
                ]
            return (e, (x, y), t)
--            return (e, (x - ((diWidth dim) `div` 2), y - ((diHeight dim) `div` 2)), t)

    case ppB of
        Nothing -> do
            pes <- liftIO $ mapM (createPE Pixel) ppA
            return $ Node (nodeType, nd) (map (\(e, p, t) -> Node (t, (setData kent e (initData kpos p))) []) pes)

        Just ppB' -> do
            let ppBoth = filter (\p ->  p `elem` ppB') ppA
            pesBoth <- liftIO $ mapM (createPE Pixel) ppBoth

            let ppAOnly = filter (\p ->  not (p `elem` ppB')) ppA
            pesA <- liftIO $ mapM (createPE PixelA) ppAOnly

            let ppBOnly = filter (\p ->  not (p `elem` ppA)) ppB'
            pesB <- liftIO $ mapM (createPE PixelB) ppBOnly

            return $ Node (nodeType, nd) (map (\(e, p, t) -> Node (t, (setData kent e (initData kpos p))) []) (pesBoth ++ pesA ++ pesB))

createLevel :: [BuildElement] -> ReaderStateIO MoaR MoaS ()
createLevel bd = do

    (hg3d, screenA, musicA, kent, kdim, kpos, khits, kanim) <- lift ask
    (cycles, _, _, _) <- get

    nodes <- mapM (\be -> case be of
                            BEOne nt pix -> createMoveNode nt pix
                            BERow nt pix@(x, y) space count -> do
                                let (nt', nt'') = case nt of
                                        (Invader n) -> (InvaderRow, Invader n)
                                        Boulder -> (BoulderRow, Boulder)
                                        Canon -> (CanonRow, ReserveCanon)
                                        Shot -> (ShotRow, Shot)
                                subnodes <- mapM (\pix' -> createMoveNode nt'' pix') [(x' + x, y) | x' <- [0, space .. ((count-1)*space)]] 
                                return (Node (nt', (HM.singleton kpos pix)) subnodes) 
                    ) bd 

    let tree = Node (Empty, HM.empty) nodes
    put (cycles, (0, -65), (0, 0), tree)
    return ()

-- moving

moveNode :: NodeData -> PixelPos -> ReaderStateIO MoaR MoaS NodeData
moveNode nodeData (x', y') = do

    (hg3d, screenA, musicA, kent, kdim, kpos, khits, kanim) <- lift ask

    let (x, y) = nodeData ! kpos
    let nodeData' = setData kpos (x + x', y + y') nodeData
    let e = nodeData ! kent
    let di = nodeData ! kdim
    liftIO $ setC e ctPosition $ posFromPixelPos di (x + x', y + y')
    return nodeData'

nWait = 20 :: Int
movesRightLeft = 10 :: Int
movesDown = 15 :: Int

invaderMove :: NodeData -> Bool -> Int -> ReaderStateIO MoaR MoaS NodeData
invaderMove nd bMove move = do

    if bMove 
        then if move < movesRightLeft 
                    then moveNode nd (2,0) 
                    else if move == movesRightLeft
                            then moveNode nd (0, -5) 
                            else if move <= 2 * movesRightLeft
                                        then moveNode nd (-2, 0) 
                                        else moveNode nd (0, -5)
        else return nd


-- shooting

isActiveShot :: NodeData -> KPos -> Bool
isActiveShot nd k = let
    (x, y) = nd ! k
    in x > (-500)

deactivateShot :: NodeData -> KPos -> NodeData
deactivateShot nd kpos = setData kpos (-1000, 0) nd

shoot :: ReaderStateIO MoaR MoaS Bool
shoot = do
    (hg3d, screenA, musicA, kent, kdim, kpos, khits, kanim) <- lift ask
    (cycles, canonPos, canonMove, gameData) <- get
    let (x, y) = canonPos
    let shootPos = (x, y + 5)
    let ((found', nodeData'), gameData') = Tr.mapAccumL (\(found, nd) (nodeType, nodeData) -> if (not found && nodeType == Shot)
                        then
                            if not (isActiveShot nodeData kpos)
                                then ((True, nodeData), (Shot, setData kpos shootPos nodeData))
                                else ((False, undefined), (nodeType, nodeData))
                        else ((found, nd), (nodeType, nodeData))
                  ) (False, undefined) gameData

    if found'
        then do
            liftIO $ setC (nodeData' ! kent) ctPosition $ posFromPixelPos (nodeData' ! kdim) shootPos
            put (cycles, canonPos, canonMove, gameData')
        else return ()

    return found'


