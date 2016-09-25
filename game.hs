{-# LANGUAGE OverloadedStrings #-}

module Main where

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
import Input
import Move
import Canon
import Music
import Switch
import Status
import Fly
import Collision
import Animate


-- MAIN PROGRAM
-- ------------

gameLogic :: HG3D -> IO ()
gameLogic hg3d = do

    keys <- genKeys

    -- intialize cam
    cam <- initializeCam hg3d

    -- create actors
    [moveA, canonA, collA, flyingA, musicA, switchA, keyA, statusBarA, animateA] <- mapM (const newActor) [1..9]

    -- interconnect and run them
    runActor animateA animateActorF (hg3d, keys) (0, undefined)
    runActor statusBarA statusBarActorF hg3d (0, undefined, undefined, undefined)
    runActor flyingA flyingActorF (hg3d, cam) (undefined, undefined)
    runActor musicA musicActorF hg3d (undefined, undefined, undefined, undefined)
    runActor collA collisionActorF (switchA, keys) ()
    runActor canonA canonActorF (hg3d, switchA, musicA, collA, keys) ((0,-65), (0,0), False)
    runActor moveA movementActorF (hg3d, moveA, switchA, musicA, collA, statusBarA, keys) (0)
    runActor switchA gameSwitchActorF (hg3d, animateA, moveA, canonA, collA, musicA, flyingA, statusBarA) (undefined, Nothing, Nothing, Nothing, undefined, ProgramInitializing)
    runActor keyA keyInputActorF (hg3d, switchA) (undefined, [])

    let cycleLoop n m = do
            if n == 0 
                then sendMsg switchA SlowCycle
                else return ()
            sendMsg keyA PollKeys
            sendMsg switchA FastCycle
            sleepFor (msecT 30)
            cycleLoop (if n == 0 then m else n - 1) m

    forkIO $ cycleLoop 0 10

    -- start with game logic by starting first switch
    sendMsg switchA StartProgram

    return ()


main = do
    runGame standardGraphics3DConfig gameLogic (msecT 20)
    return ()

initializeCam hg3d = do
    -- create minimum elements, like a camera and a light
    eCam <- newE hg3d [
        ctCamera #: FullViewCamera,
        ctPosition #: Vec3 1 1 (-30.0),
        ctLight #: Light PointLight 1.0 1000.0 1.0,
        ctOrientation #: unitU 
        ]
    return eCam


