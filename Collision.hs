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

type CoaR = (Actor, Keys)
type CoaS = ()  

mapAccumLM f a xs = runStateT (Tr.mapM (StateT . f) xs) a

collisionActorF :: Message -> ReaderStateIO CoaR CoaS ()
collisionActorF msg = do

    (screenA, keys) <- lift ask

    case msg of

        InitActor -> return ()

        CollisionStep canonData invaderData -> do
            liftIO $ runCollisionDetection keys invaderData canonData screenA
            return ()


isCollision :: Keys -> NodeData -> NodeData -> Bool
isCollision keys i s = let
    (kent, kdim, kpos, khits, kanim, kuni) = keys
    (x, y) = i ! kpos
    (w, h) = (diWidth (i ! kdim), diHeight (i ! kdim))
    (x', y') = s ! kpos
    (w', h') = (diWidth (s ! kdim), diHeight (s ! kdim))

    xmin = min (x - (w `div` 2)) (x' - (w' `div` 2))
    xmax = max (x + (w `div` 2)) (x' + (w' `div` 2))
    ymin = min (y - abs (h `div` 2)) (y' - abs (h' `div` 2))
    ymax = max (y + abs (h `div` 2)) (y' + abs (h' `div` 2))

    wg = xmax - xmin
    hg = ymax - ymin

    coll = (wg + 1) <= (w' + w) && (hg - 3) <= (h' + h)
    in coll

removeDuplicates = foldr (\x seen -> if x `elem` seen then seen else x : seen) []

runCollisionDetection :: Keys -> GameData -> GameData -> Actor -> IO ()
runCollisionDetection keys invaderData canonData screenA = do
    let (kent, kdim, kpos, khits, kanim, kuni) = keys
    let invs = filter (\(nt, nd) -> case nt of
            (Invader _) -> let (x, y) = nd ! kpos in if x < (-500) then False else True
            Boulder -> let (x, y) = nd ! kpos in if x < (-500) then False else True
            _ -> False
            ) (flatten invaderData)
    let shots = filter (\(nt, nd) -> nt == Shot) (flatten canonData)
    let cols = [ (s ! kuni, i ! kuni) | (_, s) <- shots, (_, i) <- invs, isCollision keys i s]
    let cols' = removeDuplicates $ concatMap (\(a, b) -> [a, b]) cols

    sendMsg screenA $ ActualCollData cols'
    return ()
