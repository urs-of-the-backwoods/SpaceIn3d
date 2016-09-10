{-# LANGUAGE OverloadedStrings #-}

module Screen where

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


-- SCREEN ACTOR
-- ------------

data GameState = ProgramInitializing | InitScreen | BuildField | PlayGame | Flying | FinalScore deriving (Eq, Ord, Show)

type GsaR = (HG3D, Actor, Actor, Actor, Actor)
type GsaS = ((Entity, Entity, Entity, Entity, Entity), T.Text, GameState)

gameScreenActorF :: Message -> ReaderStateIO GsaR GsaS ()
gameScreenActorF msg = do

    (hg3d, moveA, musicA, flyingA, statusBarA) <- lift ask
    (screenText, name, gameState) <- get

    let returnStay = return () 
    let returnMoveTo state = put (screenText, name, state) >> return ()

    case gameState of

        ProgramInitializing -> 
            case msg of
                StartProgram -> do
                    (eT1, eT2, eT3, eT4, eName) <- liftIO $ showInitScreen hg3d
                    liftIO $ sendMsg musicA StartMusic
                    put ((eT1, eT2, eT3, eT4, eName), name, InitScreen) >> return ()
                _ -> returnStay

        InitScreen -> 

            case msg of

                SingleKey k -> do
                    if k == "Return" 
                        then do
                            let (eT1, eT2, eT3, eT4, eName) = screenText
                            name' <- liftIO $ readC eName ctEditText
                            liftIO $ sendMsg moveA (BuildLevel buildData1)
                            liftIO $ sendMsg musicA StopMusic
                            liftIO $ mapM (\e -> setC e ctScreenRect (Rectangle (-1000) (-1000) 0 0)) [eT1, eT2, eT3, eT4, eName]
                            put (undefined, name', BuildField) >> return ()
                        else returnStay

                _ -> returnStay

        BuildField -> 
            case msg of
                BuildDone -> do
                    liftIO $ sendMsg statusBarA (SetName name)
                    liftIO $ sendMsg statusBarA (SetCount 0)
                    liftIO $ sendMsg statusBarA (SetMode "playing")
                    liftIO $ sendMsg statusBarA (DisplayStatus)
                    returnMoveTo PlayGame
                _ -> returnStay

        PlayGame -> 

            case msg of

                SingleKey k -> do
                    case k of
                        "Space" -> do
                            liftIO $ sendMsg moveA Shoot
                            returnStay
                        "F1" -> liftIO (sendMsg statusBarA (SetMode "paused ...")) >> returnMoveTo Flying
                        "F2" -> liftIO (sendMsg flyingA ResetCamPosition) >> returnStay
                        _ -> returnStay

                MovementCycle -> liftIO (sendMsg moveA MovementCycle) >> returnStay

                KeysPressed keys -> do
                    if ("Left" `elem` keys) && (not ("Right" `elem` keys))
                        then do
                            liftIO $ sendMsg moveA MoveLeft
                            returnStay
                        else returnStay

                    if ("Right" `elem` keys) && (not ("Left" `elem` keys)) 
                        then do
                            liftIO $ sendMsg moveA MoveRight
                            returnStay
                        else returnStay

                _ -> returnStay


        Flying -> 

            case msg of

                SingleKey k -> do
                    case k of
                        "F1" -> liftIO (sendMsg statusBarA (SetMode "playing")) >> returnMoveTo PlayGame
                        "W" -> liftIO (sendMsg flyingA MoreSpeed) >> returnStay
                        "S" -> liftIO (sendMsg flyingA LessSpeed) >> returnStay
                        "Q" -> liftIO (sendMsg flyingA ZeroSpeed) >> returnStay
                        "F2" -> liftIO (sendMsg flyingA ResetCamPosition) >> returnStay
                        "F3" -> liftIO (sendMsg flyingA SaveCamPosition) >> returnStay
                        "F4" -> liftIO (sendMsg flyingA RestoreCamPosition) >> returnStay
                        _ -> returnStay

                KeysPressed keys -> do
                    mapM (\k -> do
                        case k of
                            "A" -> liftIO $ sendMsg flyingA YawLeft
                            "D" -> liftIO $ sendMsg flyingA YawRight
                            "Up" -> liftIO $ sendMsg flyingA PitchUp 
                            "Down" -> liftIO $ sendMsg flyingA PitchDown
                            "Left" -> liftIO $ sendMsg flyingA RollLeft 
                            "Right" -> liftIO $ sendMsg flyingA RollRight
                            _ -> return ()
                        ) keys
                    returnStay

                _ -> returnStay



showInitScreen hg3d = do

    eT1 <- newE hg3d [
        ctText #: "Space Invaders 3D",
        ctScreenRect #: Rectangle 250 100 100 25
        ]

    eT2 <- newE hg3d [
        ctText #: "programmed by: Peter Althainz\nusing the fabulous HGamer3D toolset\nSeptember 2016\n\nhttp://www.hgamer3d.org",
        ctScreenRect #: Rectangle 220 150 100 60
        ]

    eT3 <- newE hg3d [
        ctText #: "dear brave hero, please type in your name: ",
        ctScreenRect #: Rectangle 220 250 100 60
        ]

    eName <- newE hg3d [
        ctEditText #: "The Brave Hero",
        ctScreenRect #: Rectangle 220 280 200 30
        ]

    eT4 <- newE hg3d [
        ctText #: (T.pack . unlines $ [
            "Keys:",
            "F1 - switch pause flight mode - play mode",
            "F2 - reset camera position",
            "F3 - store camera position (flight mode)",
            "F4 - restore camera position (flight mode)\n",
            "Flight Mode: WSADQ Up/Down/Left/Right",
            "Play Mode: Left/Right/Space" ]),
        ctScreenRect #: Rectangle 220 350 100 60
        ]

    return (eT1, eT2, eT3, eT4, eName)


