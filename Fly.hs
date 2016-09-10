{-# LANGUAGE OverloadedStrings #-}

module Fly (
        flyingActorF
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


-- FLYING CONTROL ACTOR
-- --------------------

type FaR = (HG3D, Entity)
type FaS = (Var Speed, Var (Position, Orientation))

flyingActorF :: Message -> ReaderStateIO FaR FaS ()
flyingActorF msg = do

    let f = 0.01
    (hg3d, cam) <- lift ask
    (speed, campos) <- get

    case msg of

        InitActor -> do
            speed' <- liftIO $ makeVar (Speed 0)
            ori <- liftIO $ readC cam ctOrientation
            pos <- liftIO $ readC cam ctPosition
            campos' <- liftIO $ makeVar (pos, ori)

            let loop = do
                    (Speed s) <- readVar speed'
                    if s /= 0 then forward cam ((fromIntegral s)/30.0) else return ()
                    sleepFor (msecT 30)
                    loop

            liftIO $ forkIO $ forever $ loop
            put (speed', campos')
            return ()

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


