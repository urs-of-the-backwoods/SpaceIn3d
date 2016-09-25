{-# LANGUAGE OverloadedStrings #-}

module Canon (
    newCanonActor
) where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import qualified Data.Traversable as Tr
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data
import Actor


-- CANON ACTOR
-- -----------

data MyActors = MyActors {
    collA :: Actor,
    musicA :: Actor
}

newCanonActor :: Actor -> Actor -> Keys -> IO Actor
newCanonActor collA musicA keys = do
    let myActors = MyActors collA musicA
    actor <- newActor
    runActor actor canonActorF (myActors, keys) ((0, -65), (0, 0), False)
    return actor

type CaaR = (MyActors, Keys)
type CaaS = (PixelPos, PixelPos, Bool)

mapAccumLM f a xs = runStateT (Tr.mapM (StateT . f) xs) a

canonActorF :: Actor -> Message -> ReaderStateIO CaaR CaaS ()
canonActorF canonA m = do

    (myActors, keys) <- lift ask
    let (kent, kdim, kpos, khits, kanim, kuni) = keys
    (canonPos, canonMove, shootNow) <- get

    case m of

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
                                    then liftIO $ sendMsg (musicA myActors) PlayShot
                                    else liftIO $ sendMsg (musicA myActors) PlayNoShot
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

            liftIO $ sendMsg (collA myActors) $ ActualCanonData gameData''
            put ((x+x', y), (0,0), False) 

        _ -> return ()

moveNode' :: NodeData -> PixelPos -> ReaderStateIO CaaR CaaS NodeData
moveNode' nd p = do
    (myActors, keys) <- lift ask
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
    (myActors, keys) <- lift ask
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

