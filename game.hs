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
import Music
import Screen
import Status
import Fly


-- MAIN PROGRAM
-- ------------

gameLogic :: HG3D -> IO ()
gameLogic hg3d = do

    -- create basic keys
    kent <- HM.createKey
    kdim <- HM.createKey
    kpos <- HM.createKey
    khits <- HM.createKey
    kanim <- HM.createKey

    -- intialize cam
    cam <- initializeCam hg3d

    -- create actors
    [moveA, flyingA, musicA, screenA, keyA, statusBarA] <- mapM (const newActor) [1..6]

    -- interconnect and run them
    runActor statusBarA statusBarActorF hg3d (undefined, undefined, undefined)
    runActor flyingA flyingActorF (hg3d, cam) (undefined, undefined)
    runActor musicA musicActorF hg3d (undefined, undefined, undefined)
    runActor moveA movementActorF (hg3d, screenA, musicA, kent, kdim, kpos, khits, kanim) (0, undefined, undefined, undefined)
    runActor screenA gameScreenActorF (hg3d, moveA, musicA, flyingA, statusBarA) (undefined, undefined, ProgramInitializing)
    runActor keyA keyInputActorF (hg3d, screenA) (undefined, [])

    -- generating messages to keep game rolling
    forkIO $ forever $ do
        sendMsg keyA PollKeys
        sendMsg screenA MovementCycle
        sleepFor (msecT 15)
        return ()

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


