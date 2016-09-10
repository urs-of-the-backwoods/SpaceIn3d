{-# LANGUAGE OverloadedStrings #-}

module Canon (
    canonActorF
) where

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


-- CANON ACTOR
-- -----------

buildCanonData :: [BuildElement]
buildCanonData = [
        BERow Boulder (-45, -50) 40 4,
        BEOne Canon (0, -65),
        BERow Canon (-60, -80) 15 2,
        BERow Shot (-1000, 0) 5 5
    ]

type CaaR = (HG3D, Actor, Actor, Actor, Keys)
type CaaS = (PixelPos, PixelPos, GameData)

mapAccumLM f a xs = runStateT (Tr.mapM (StateT . f) xs) a

canonActorF :: Message -> ReaderStateIO CaaR CaaS ()
canonActorF m = do

    (hg3d, screenA, musicA, collA, keys) <- lift ask
    let (kent, kdim, kpos, khits, kanim, kuni) = keys
    (canonPos, canonMove, gameData) <- get

    case m of
        Collision sid -> do
            gameData' <- mapM (\(nodeType, nodeData) -> do
                nodeData' <- case nodeType of
                    Shot -> do
                        let i = nodeData ! kuni
                        if i == sid
                            then return (deactivateShot nodeData kpos)
                            else return nodeData
                    _ -> return nodeData
                return (nodeType, nodeData')
                ) gameData
            put (canonPos, canonMove, gameData') 

        BuildLevel -> do
            createCanonLevel buildCanonData
            liftIO  $ sendMsg screenA BuildDone
            return ()

        MoveLeft -> put (canonPos, (-1, 0), gameData)
        MoveRight ->  put (canonPos, (1, 0), gameData)

        Shoot -> do
            bulletAvailable <- shoot
            if bulletAvailable 
                then liftIO $ sendMsg musicA PlayShot
                else liftIO $ sendMsg musicA PlayNoShot
            return ()

        FastCycle -> do

            -- canon movement
            let (x, y) = canonPos
            let (x', y') = canonMove

            -- general movement
            gameData' <- mapM (\(nodeType, nodeData) -> do
                nodeData' <- case nodeType of
                    Canon -> moveNode' nodeData canonMove
                    Shot -> do
                        nd <- moveNode' nodeData (0, 3)
                        let (x, y) = nd ! kpos
                        if y > 100 
                            then return (deactivateShot nd kpos)
                            else return nd
                    _ -> return nodeData
                return (nodeType, nodeData')
                ) gameData

            liftIO $ sendMsg collA $ ActualCanonData gameData'
            put ((x+x', y), (0,0), gameData') 

        _ -> return ()

createCanonLevel :: [BuildElement] -> ReaderStateIO CaaR CaaS ()
createCanonLevel bd = do

    (hg3d, screenA, musicA, collA, keys) <- lift ask
    let (kent, kdim, kpos, khits, kanim, kuni) = keys
    tree <- liftIO $ gameDataFromBuildData hg3d keys bd
    put ((0, -65), (0, 0), tree)
    return ()

moveNode' :: NodeData -> PixelPos -> ReaderStateIO CaaR CaaS NodeData
moveNode' nd p = do
    (hg3d, screenA, musicA, collA, keys) <- lift ask
    nd <- liftIO $ moveNode keys nd p
    return nd

-- shooting

isActiveShot :: NodeData -> KPos -> Bool
isActiveShot nd k = let
    (x, y) = nd ! k
    in x > (-500)

deactivateShot :: NodeData -> KPos -> NodeData
deactivateShot nd kpos = setData kpos (-1000, 0) nd

shoot :: ReaderStateIO CaaR CaaS Bool
shoot = do
    (hg3d, screenA, musicA, collA, keys) <- lift ask
    let (kent, kdim, kpos, khits, kanim, kuni) = keys
    (canonPos, canonMove, gameData) <- get
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
            put (canonPos, canonMove, gameData')
        else return ()

    return found'


