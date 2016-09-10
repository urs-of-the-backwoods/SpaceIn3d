{-# LANGUAGE OverloadedStrings #-}

module Actor where

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

-- ACTORS
-- ------

-- define all messages in this program, high level between actors

data Message = InitActor | StopActor -- intialize and stop actor
             | InitMusic | StartMusic | StopMusic | PlayShot | PlayNoShot | PlayExplosion -- music actor
             | StartProgram | BuildDone | KeysPressed [T.Text] | SingleKey T.Text -- screen actor
             | InitKeys | PollKeys -- key input actor
             | BuildLevel [BuildElement] | MoveLeft | MoveRight | Shoot | MovementCycle -- movement actor
             | RollRight | RollLeft | PitchUp | PitchDown -- flying control
             | YawLeft | YawRight 
             | MoreSpeed | LessSpeed | ZeroSpeed 
             | ResetCamPosition | RestoreCamPosition | SaveCamPosition
             | DisplayStatus | HideStatus | SetName T.Text | SetCount Int | SetMode T.Text -- status bar actor
             deriving (Eq, Ord, Show)

-- we are going for an actor model with forkIO starting an actor and MVar being 
-- the message passing channel, actors are bigger pieces of logic 

newtype Actor = Actor (MVar Message)

newActor :: IO Actor
newActor = do
    mv <- newEmptyMVar
    return (Actor mv)

type ReaderStateIO r s a = StateT s (ReaderT r IO) a

runActor :: Actor -> (Message -> ReaderStateIO r s () ) -> r -> s -> IO ()
runActor a@(Actor mv) f ri si = do
    let loop mv s = do
            msg <- takeMVar mv
            (_, s') <- runReaderT (runStateT (f msg) s) ri
            loop mv s'
    forkIO $ loop mv si
    sendMsg a InitActor

stopActor a = sendMsg a StopActor

sendMsg :: Actor -> Message -> IO ()
sendMsg (Actor mv) m = putMVar mv m

