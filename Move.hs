{-# LANGUAGE OverloadedStrings #-}

module Move (
        movementActorF
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

import Data.Unique
import Debug.Trace

import Data
import Actor


-- MOVEMENT ACTOR
-- --------------

buildInvadersData :: [BuildElement]
buildInvadersData = [
        BEOne Ship (0, 100),
        BERow (Invader 3) (-60, 85) 15 11,
        BERow (Invader 2) (-60, 70) 15 11,
        BERow (Invader 2) (-60, 55) 15 11,
        BERow (Invader 1) (-60, 40) 15 11,
        BERow (Invader 1) (-60, 25) 15 11
    ]

type MoaR = (HG3D, Actor, Actor, Actor, Keys)
type MoaS = (Int, MVar GameData, [Unique])

mapAccumLM f a xs = runStateT (Tr.mapM (StateT . f) xs) a

movementActorF :: Message -> ReaderStateIO MoaR MoaS ()
movementActorF m = do

    (hg3d, screenA, musicA, collA, keys) <- lift ask
    let (kent, kdim, kpos, khits, kanim, kuni) = keys
    (c, lastGameData, colls) <- get

    case m of

        Collision iid -> put (c, lastGameData, iid : colls)

        InitActor -> do
            mv <- liftIO $ newEmptyMVar
            put (c, mv, colls)

        BuildLevel -> do
            createLevel buildInvadersData
            liftIO  $ sendMsg screenA BuildDone
            return ()

        SlowCycle -> do
            -- try to get result from last run
            let moves = c `div` nWait + (movesRightLeft `div` 2) -- start in the middle 
            let bMove = (c `mod` nWait == 0) && ((c `div` nWait) < (( 2 * movesRightLeft + 2 ) * movesDown) ) 
            let move = moves `mod` (2 * movesRightLeft + 2)
            put (c + 1, lastGameData, colls)

            if bMove 
                then do
                    mbGameData <- liftIO $ tryTakeMVar lastGameData
                    case mbGameData of
                        Nothing -> liftIO (print "lost cycle") >> return ()
                        Just gameData -> do
                            put (c + 1, lastGameData, [])
                            liftIO $ forkIO $ doOneRun keys collA lastGameData gameData colls move moves
                            return ()
                else return ()

        _ -> return ()


doOneRun :: Keys -> Actor -> MVar GameData -> GameData -> [Unique] -> Int -> Int -> IO ()
doOneRun keys collA lastGameData gameData colls move moves = do

    let (kent, kdim, kpos, khits, kanim, kuni) = keys

    -- general movement
    gameData' <- mapM (\(nodeType, nodeData) -> do
                        nodeData' <- case nodeType of
                            -- invaders stepping
                            (Invader _) -> do 
                                let i = nodeData ! kuni
                                if i `elem` (colls) 
                                    then do
                                        let nodeData' = deactivateInvader nodeData kpos
                                        liftIO $ moveNode keys nodeData' (nodeData' ! kpos)
                                        return nodeData'
                                    else invaderMove keys nodeData move
                            _ -> return nodeData
                        return (nodeType, nodeData')
                        ) gameData

    -- pixel animation
    (gameData'', _) <- mapAccumLM (\(nodeType, nodeData) (nt, nd) -> case nodeType of
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

            ) (undefined, undefined) gameData'

    sendMsg collA $ ActualInvaderData gameData''
    putMVar lastGameData gameData''


createLevel :: [BuildElement] -> ReaderStateIO MoaR MoaS ()
createLevel bd = do

    (hg3d, screenA, musicA, collA, keys) <- lift ask
    let (kent, kdim, kpos, khits, kanim, kuni) = keys
    (cycles, lastGameData, colls) <- get
    tree <- liftIO $ gameDataFromBuildData hg3d keys bd
    liftIO $ putMVar lastGameData tree
    put (cycles, lastGameData, colls)
    return ()

nWait = 4 :: Int
movesRightLeft = 10 :: Int
movesDown = 15 :: Int

invaderMove :: Keys -> NodeData -> Int -> IO NodeData
invaderMove keys nd move = do

    let (kent, kdim, kpos, khits, kanim, kuni) = keys

    if move < movesRightLeft 
        then moveNode keys nd (2,0) 
        else if move == movesRightLeft
                then moveNode keys nd (0, -5) 
                else if move <= 2 * movesRightLeft
                            then moveNode keys nd (-2, 0) 
                            else moveNode keys nd (0, -5)


deactivateInvader :: NodeData -> KPos -> NodeData
deactivateInvader nd kpos = setData kpos (-1000, 0) nd


