{-# LANGUAGE OverloadedStrings #-}

module Collision (
        collisionActorF
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


-- COLLISION ACTOR
-- ---------------
-- computes collisions and sends back info to other actors 

type CoaR = (Actor, Actor, Actor, Keys)
type CoaS = (Maybe GameData, Maybe GameData, MVar ())   -- as soon as both are filled, we are computing a new collision

mapAccumLM f a xs = runStateT (Tr.mapM (StateT . f) xs) a

collisionActorF :: Message -> ReaderStateIO CoaR CoaS ()
collisionActorF msg = do

    (moveA, canonA, statusA, keys) <- lift ask
    (invaderData, canonData, busyFlag) <- get

    case msg of

        InitActor -> do
            mv <- liftIO $ newMVar ()
            put (invaderData, canonData, mv)

        FastCycle -> case (invaderData, canonData) of
            (Just invaderData', Just canonData') -> do
                mBF <- liftIO $ tryTakeMVar busyFlag
                case mBF of
                    Just () -> do
                        liftIO $ forkIO $ runCollisionDetection keys invaderData' canonData' busyFlag moveA canonA statusA
                        return ()
                    Nothing -> return ()
            _ -> return ()

        ActualInvaderData invaderData' -> put (Just invaderData', canonData, busyFlag)
        ActualCanonData canonData' -> put (invaderData, Just canonData', busyFlag)


isCollision :: Keys -> NodeData -> NodeData -> Bool
isCollision keys i s = let
    (kent, kdim, kpos, khits, kanim, kuni) = keys
    (x, y) = i ! kpos
    (w, h) = (diWidth (i ! kdim), diHeight (i ! kdim))
    (x', y') = s ! kpos
    (w', h') = (diWidth (s ! kdim), diHeight (s ! kdim))

    xmin = min (x - w `div` 2) (x' - w' `div` 2)
    xmax = max (x + w `div` 2) (x' + w' `div` 2)
    ymin = min (y - h `div` 2) (y' - h' `div` 2)
    ymax = max (y + h `div` 2) (y' + h' `div` 2)

    wg = xmax - xmin
    hg = ymax - ymin

    coll = wg <= (w' + w) && hg <= (h' + h)
    in coll

runCollisionDetection :: Keys -> GameData -> GameData -> MVar () -> Actor -> Actor -> Actor -> IO ()
runCollisionDetection keys invaderData canonData busyFlag moveA canonA statusA = do
    let (kent, kdim, kpos, khits, kanim, kuni) = keys
    let invs = filter (\(nt, nd) -> case nt of
            (Invader _) -> True
            _ -> False
            ) (flatten invaderData)
    let shots = filter (\(nt, nd) -> nt == Shot) (flatten canonData)
    let cols = [ (s ! kuni, i ! kuni) | (_, s) <- shots, (_, i) <- invs, isCollision keys i s]

    if length cols > 0 
        then do
            (mapM ( \(sid, iid) -> do
                sendMsg canonA $ Collision sid
                sendMsg moveA $ Collision iid
                ) cols) >> return ()
        else return ()

    putMVar busyFlag ()
    return ()
