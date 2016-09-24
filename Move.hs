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
        BERow Boulder (-45, -50) 40 4,
        BERow (Invader 3) (-60, 85) 15 11,
        BERow (Invader 2) (-60, 70) 15 11,
        BERow (Invader 2) (-60, 55) 15 11,
        BERow (Invader 1) (-60, 40) 15 11,
        BERow (Invader 1) (-60, 25) 15 11
    ]

type MoaR = (HG3D, Actor, Actor, Actor, Actor, Actor, Keys)
type MoaS = (Int)

mapAccumLM f a xs = runStateT (Tr.mapM (StateT . f) xs) a

movementActorF :: Message -> ReaderStateIO MoaR MoaS ()
movementActorF m = do

    (hg3d, moveA, screenA, musicA, collA, statusA, keys) <- lift ask
    let (kent, kdim, kpos, khits, kanim, kuni) = keys
    (c) <- get

    case m of

        InitActor -> return ()

        BuildLevel -> do
            gameData <- createLevel buildInvadersData
            liftIO $ sendMsg screenA (ActualInvaderData gameData)
            liftIO $ sendMsg screenA BuildDone
            return ()

        MoveStep gameData colls -> do
            let moves = c `div` nWait + (movesRightLeft `div` 2) -- start in the middle 
            let bMove = (c `mod` nWait == 0) && ((c `div` nWait) < (( 2 * movesRightLeft + 2 ) * movesDown) ) 
            let move = moves `mod` (2 * movesRightLeft + 2)
            put (c + 1)

            if bMove 
                then do
                    liftIO $ doOneRun keys statusA screenA musicA gameData colls move moves
                    return ()
                else do
                    liftIO $ removeColls keys statusA screenA musicA gameData colls move moves
                    return ()

        _ -> return ()


removeColls :: Keys -> Actor -> Actor -> Actor -> GameData -> [Unique] -> Int -> Int -> IO ()
removeColls keys statusA screenA musicA gameData colls move moves = do

    let (kent, kdim, kpos, khits, kanim, kuni) = keys

    gameData' <- mapM (\(nodeType, nodeData) -> do
                        nodeData' <- case nodeType of
                            (Invader _) -> do 
                                let i = nodeData ! kuni
                                if i `elem` (colls) 
                                    then do
                                        sendMsg musicA PlayExplosion
                                        nodeData' <- liftIO $ deactivateInvader statusA nodeType nodeData kpos
                                        liftIO $ moveNode keys nodeData' (nodeData' ! kpos)
                                        return nodeData'
                                    else return nodeData
                            _ -> return nodeData
                        return (nodeType, nodeData')
                        ) gameData

    sendMsg screenA $ ActualInvaderData gameData'


doOneRun :: Keys -> Actor -> Actor -> Actor -> GameData -> [Unique] -> Int -> Int -> IO ()
doOneRun keys statusA screenA musicA gameData colls move moves = do

    let (kent, kdim, kpos, khits, kanim, kuni) = keys

    gameData' <- mapM (\(nodeType, nodeData) -> do
                        nodeData' <- case nodeType of
                            (Invader _) -> do 
                                let i = nodeData ! kuni
                                if i `elem` (colls) 
                                    then do
                                        sendMsg musicA PlayExplosion
                                        nodeData' <- liftIO $ deactivateInvader statusA nodeType nodeData kpos
                                        liftIO $ moveNode keys nodeData' (nodeData' ! kpos)
                                        return nodeData'
                                    else invaderMove keys nodeData move
                            _ -> return nodeData
                        return (nodeType, nodeData')
                        ) gameData
    let gameData'' = gameData'

    sendMsg screenA $ ActualInvaderData gameData''

createLevel :: [BuildElement] -> ReaderStateIO MoaR MoaS GameData
createLevel bd = do

    (hg3d, moveA, screenA, musicA, collA, statusA, keys) <- lift ask
    let (kent, kdim, kpos, khits, kanim, kuni) = keys
    (cycles) <- get
    tree <- liftIO $ gameDataFromBuildData hg3d keys bd
    put (cycles)
    return tree

nWait = 12 :: Int
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

deactivateInvader :: Actor -> NodeType -> NodeData -> KPos -> IO NodeData
deactivateInvader statusA nt nd kpos = do
    case nt of
        Invader 1 -> sendMsg statusA (AddCount 5)   -- each hit is done twice, before invaders removed
        Invader 2 -> sendMsg statusA (AddCount 10)
        Invader 3 -> sendMsg statusA (AddCount 15)
    return $ setData kpos (-1000, 0) nd

