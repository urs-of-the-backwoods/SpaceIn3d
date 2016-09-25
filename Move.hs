{-# LANGUAGE OverloadedStrings #-}

module Move (
        newMoveActor
    ) where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import qualified Data.Traversable as Tr
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Unique

import Data
import Actor


-- MOVEMENT ACTOR
-- --------------

data MyActors = MyActors {
    gameLoopA :: Actor,
    musicA :: Actor,
    statusA :: Actor
    }

newMoveActor :: HG3D -> Actor -> Actor -> Actor -> Keys -> IO Actor
newMoveActor hg3d glA mA sA keys = do
    let myActors = MyActors glA mA sA
    actor <- newActor
    runActor actor movementActorF (hg3d, myActors, keys) 0
    return actor

type MoaR = (HG3D, MyActors, Keys)
type MoaS = (Int)

mapAccumLM f a xs = runStateT (Tr.mapM (StateT . f) xs) a

movementActorF :: Actor -> Message -> ReaderStateIO MoaR MoaS ()
movementActorF moveA m = do

    (hg3d, myActors, keys) <- lift ask
    let (kent, kdim, kpos, khits, kanim, kuni) = keys
    (c) <- get

    case m of

        MoveStep gameData colls -> do
            let moves = c `div` nWait + (movesRightLeft `div` 2) -- start in the middle 
            let bMove = (c `mod` nWait == 0) && ((c `div` nWait) < (( 2 * movesRightLeft + 2 ) * movesDown) ) 
            let move = moves `mod` (2 * movesRightLeft + 2)
            put (c + 1)

            if bMove 
                then do
                    liftIO $ doOneRun keys myActors gameData colls move moves
                    return ()
                else do
                    liftIO $ removeColls keys myActors gameData colls move moves
                    return ()

        _ -> return ()


removeColls :: Keys -> MyActors -> GameData -> [Unique] -> Int -> Int -> IO ()
removeColls keys myActors gameData colls move moves = do

    let (kent, kdim, kpos, khits, kanim, kuni) = keys

    gameData' <- mapM (\(nodeType, nodeData) -> do
                        nodeData' <- case nodeType of
                            (Invader _) -> do 
                                let i = nodeData ! kuni
                                if i `elem` (colls) 
                                    then do
                                        sendMsg (musicA myActors) PlayExplosion
                                        nodeData' <- liftIO $ deactivateInvader (statusA myActors) nodeType nodeData kpos
                                        liftIO $ moveNode keys nodeData' (nodeData' ! kpos)
                                        return nodeData'
                                    else return nodeData
                            _ -> return nodeData
                        return (nodeType, nodeData')
                        ) gameData

    sendMsg (gameLoopA myActors) $ ActualInvaderData gameData'


doOneRun :: Keys -> MyActors -> GameData -> [Unique] -> Int -> Int -> IO ()
doOneRun keys myActors gameData colls move moves = do

    let (kent, kdim, kpos, khits, kanim, kuni) = keys

    gameData' <- mapM (\(nodeType, nodeData) -> do
                        nodeData' <- case nodeType of
                            (Invader _) -> do 
                                let i = nodeData ! kuni
                                if i `elem` (colls) 
                                    then do
                                        sendMsg (musicA myActors) PlayExplosion
                                        nodeData' <- liftIO $ deactivateInvader (statusA myActors) nodeType nodeData kpos
                                        liftIO $ moveNode keys nodeData' (nodeData' ! kpos)
                                        return nodeData'
                                    else invaderMove keys nodeData move
                            _ -> return nodeData
                        return (nodeType, nodeData')
                        ) gameData
    let gameData'' = gameData'

    sendMsg (gameLoopA myActors) $ ActualInvaderData gameData''


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

