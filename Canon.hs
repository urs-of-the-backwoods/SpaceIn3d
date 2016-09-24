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
        BEOne Canon (0, -65),
        BERow Canon (-60, -80) 15 2,
        BERow Shot (-1000, 0) 5 5
    ]

type CaaR = (HG3D, Actor, Actor, Actor, Keys)
type CaaS = (PixelPos, PixelPos, Bool)

mapAccumLM f a xs = runStateT (Tr.mapM (StateT . f) xs) a

canonActorF :: Message -> ReaderStateIO CaaR CaaS ()
canonActorF m = do

    (hg3d, screenA, musicA, collA, keys) <- lift ask
    let (kent, kdim, kpos, khits, kanim, kuni) = keys
    (canonPos, canonMove, shootNow) <- get

    case m of

        BuildLevel -> do
            gameData <- createCanonLevel buildCanonData
            liftIO $ sendMsg screenA $ ActualCanonData gameData
            liftIO  $ sendMsg screenA BuildDone
            return ()

        MoveLeft -> do
            let (x, y) = canonPos
            if x > -63
                then put (canonPos, (-1, 0), shootNow)
                else return ()

        MoveRight -> do
            let (x, y) = canonPos
            if x < 95
                then put (canonPos, (1, 0), shootNow)
                else return ()

        Shoot -> put (canonPos, canonMove, True) >> return ()

        CanonStep gameData colls -> do

            let (x, y) = canonPos
            let (x', y') = canonMove

            gameData' <- if shootNow
                            then do
                                (bulletAvailable, gd) <- shoot gameData
                                if bulletAvailable 
                                    then liftIO $ sendMsg musicA PlayShot
                                    else liftIO $ sendMsg musicA PlayNoShot
                                return gd
                            else return gameData

            gameData'' <- mapM (\(nodeType, nodeData) -> do
                nodeData' <- case nodeType of
                    Canon -> moveNode' nodeData canonMove
                    Shot -> do
                        let i = nodeData ! kuni
                        if i `elem` colls
                            then return (deactivateShot nodeData kpos)
                            else do
                                nd <- moveNode' nodeData (0, 3)
                                let (x, y) = nd ! kpos
                                if y > 100 
                                    then return (deactivateShot nd kpos)
                                    else return nd
                    _ -> return nodeData
                return (nodeType, nodeData')
                ) gameData'

            liftIO $ sendMsg screenA $ ActualCanonData gameData''
            put ((x+x', y), (0,0), False) 

        _ -> return ()

createCanonLevel :: [BuildElement] -> ReaderStateIO CaaR CaaS GameData
createCanonLevel bd = do

    (hg3d, screenA, musicA, collA, keys) <- lift ask
    let (kent, kdim, kpos, khits, kanim, kuni) = keys
    tree <- liftIO $ gameDataFromBuildData hg3d keys bd
    return tree

moveNode' :: NodeData -> PixelPos -> ReaderStateIO CaaR CaaS NodeData
moveNode' nd p = do
    (hg3d, screenA, musicA, collA, keys) <- lift ask
    nd <- liftIO $ moveNode keys nd p
    return nd

isActiveShot :: NodeData -> KPos -> Bool
isActiveShot nd k = let
    (x, y) = nd ! k
    in x > (-500)

deactivateShot :: NodeData -> KPos -> NodeData
deactivateShot nd kpos = setData kpos (-1000, 0) nd

shoot :: GameData -> ReaderStateIO CaaR CaaS (Bool, GameData)
shoot gameData = do
    (hg3d, screenA, musicA, collA, keys) <- lift ask
    let (kent, kdim, kpos, khits, kanim, kuni) = keys
    (canonPos, canonMove, shootNow) <- get
    let (x, y) = canonPos
    let shootPos = (x, y + 5)
    let ((found', nodeData'), gameData') = Tr.mapAccumL (\(found, nd) (nodeType, nodeData) -> if (not found && nodeType == Shot)
                        then
                            if not (isActiveShot nodeData kpos)
                                then ((True, nodeData), (Shot, setData kpos shootPos nodeData))
                                else ((found, nd), (nodeType, nodeData))
                        else ((found, nd), (nodeType, nodeData))
                  ) (False, undefined) gameData

    if found'
        then do
            liftIO $ setC (nodeData' ! kent) ctPosition $ posFromPixelPos (nodeData' ! kdim) shootPos
            return (True, gameData')
        else return (False, gameData)

