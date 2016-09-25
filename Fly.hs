{-# LANGUAGE OverloadedStrings #-}

module Fly (
        newFlyingActor
    ) where

import HGamer3D

import Control.Monad
import Control.Concurrent

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data
import Actor


-- FLYING CONTROL ACTOR
-- --------------------

type FaR = Entity
type FaS = (Var Speed, Var (Position, Orientation))

newFlyingActor :: Entity -> IO Actor
newFlyingActor cam = do

    speed' <- makeVar (Speed 0)
    ori <- readC cam ctOrientation
    pos <- readC cam ctPosition
    campos' <- makeVar (pos, ori)

    let loop = do
            (Speed s) <- readVar speed'
            if s /= 0 then forward cam ((fromIntegral s)/30.0) else return ()
            sleepFor (msecT 30)
            loop
    forkIO $ forever $ loop

    actor <- newActor
    runActor actor flyingActorF cam (speed', campos') 
    return actor

flyingActorF :: Actor -> Message -> ReaderStateIO FaR FaS ()
flyingActorF flyA msg = do

    let f = 0.01
    cam <- lift ask
    (speed, campos) <- get

    case msg of

        YawRight -> liftIO  (yaw' cam (f)) >> return ()
        YawLeft -> liftIO  (yaw' cam (-f)) >> return ()
        RollRight -> liftIO  (roll' cam (-f)) >> return ()
        RollLeft -> liftIO  (roll' cam f) >> return ()
        PitchUp -> liftIO  (pitch' cam (0.3 *f)) >> return ()
        PitchDown -> liftIO  (pitch' cam (0.3 * (-f))) >> return ()

        MoreSpeed -> liftIO  (updateVar speed (\(Speed i) -> (Speed (i + 1), ()))) >> return ()
        LessSpeed -> liftIO  (updateVar speed (\(Speed i) -> (Speed (i - 1), ()))) >> return ()
        ZeroSpeed -> liftIO  (writeVar speed (Speed 0)) >> return ()

        SaveCamPosition -> do
            ori <- liftIO $ readC cam ctOrientation
            pos <- liftIO $ readC cam ctPosition
            liftIO $ writeVar campos (pos, ori)
            liftIO $ print $ "cam position: " ++ (show pos) ++ " ori: " ++ (show ori)
            return ()

        RestoreCamPosition -> do
            (pos, ori) <- liftIO $ readVar campos
            liftIO $ setC cam ctOrientation ori
            liftIO $ setC cam ctPosition pos
            return ()

        ResetCamPosition -> do
            liftIO $ setC cam ctOrientation unitU
            liftIO $ setC cam ctPosition (Vec3 1 1 (-30.0))
            return ()


yaw' e val = updateC e ctOrientation (\ori -> yaw ori (Rad val))
roll' e val = updateC e ctOrientation (\ori -> roll ori (Rad val))
pitch' e val = updateC e ctOrientation (\ori -> pitch ori (Rad val))


-- function, to move into direction of flight
forward :: Entity -> Float -> IO ()
forward e val = do
    qob <- readC e ctOrientation
    -- this points towards nose
    let vdir = actU qob vec3Z
    updateC e ctPosition (\pos -> pos &+ (val *& vdir))
    return ()


