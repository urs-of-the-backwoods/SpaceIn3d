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
import Screen
import Status
import Fly
import Collision


-- MAIN PROGRAM
-- ------------

gameLogic :: HG3D -> IO ()
gameLogic hg3d = do

    keys <- genKeys

    -- intialize cam
    cam <- initializeCam hg3d

    -- create actors
    [moveA, canonA, collA, flyingA, musicA, screenA, keyA, statusBarA] <- mapM (const newActor) [1..8]

    -- interconnect and run them
    runActor statusBarA statusBarActorF hg3d (undefined, undefined, undefined)
    runActor flyingA flyingActorF (hg3d, cam) (undefined, undefined)
    runActor musicA musicActorF hg3d (undefined, undefined, undefined)
    runActor collA collisionActorF (moveA, canonA, statusBarA, keys) (Nothing, Nothing, undefined)
    runActor canonA canonActorF (hg3d, screenA, musicA, collA, keys) (undefined, undefined, undefined)
    runActor moveA movementActorF (hg3d, screenA, musicA, collA, keys) (0, undefined, [])
    runActor screenA gameScreenActorF (hg3d, moveA, canonA, collA, musicA, flyingA, statusBarA) (undefined, undefined, ProgramInitializing)
    runActor keyA keyInputActorF (hg3d, screenA) (undefined, [])

    let cycleLoop n m = do
            if n == 0 
                then sendMsg screenA SlowCycle
                else return ()
            sendMsg keyA PollKeys
            sendMsg screenA FastCycle
            sleepFor (msecT 30)
            cycleLoop (if n == 0 then m else n - 1) m

    forkIO $ cycleLoop 0 3

    -- start with game logic by starting first screen
    sendMsg screenA StartProgram

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


