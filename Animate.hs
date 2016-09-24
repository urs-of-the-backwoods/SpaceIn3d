{-# LANGUAGE OverloadedStrings #-}

module Animate (
        animateActorF
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

import Data.Unique
import Debug.Trace

import Data
import Actor


-- ANIMATION ACTOR
-- ---------------

type AnaR = (HG3D, Keys)
type AnaS = (Int, GameData)

mapAccumLM f a xs = runStateT (Tr.mapM (StateT . f) xs) a

animateActorF :: Message -> ReaderStateIO AnaR AnaS ()
animateActorF msg = do

    (count, gameData) <- get
    put (count + 1, gameData)

    case msg of

        InitActor -> return ()
        CacheLevel level -> put (count + 1, level)
        SlowCycle -> animateLevel
        _ -> return ()


animateLevel :: ReaderStateIO AnaR AnaS ()
animateLevel = do

    (hg3d, keys) <- lift ask
    let (kent, kdim, kpos, khits, kanim, kuni) = keys
    (count, gameData) <- get

    (gameData', _) <- liftIO $ mapAccumLM (\(nodeType, nodeData) (nt, nd) -> case nodeType of
                (Invader _) -> do
                    let ai = nodeData ! kanim
                    let ai' = getCurrentAnimation ai count
                    let nodeData' = setData kanim ai' nodeData
                    return ((nodeType, nodeData'), (nodeType, nodeData')) -- set acc to nodeData of Invader

                PixelA -> do
                    let ai = nd ! kanim         -- accu, this is the previous Invader node info!
                    if aiSwapNow ai
                        then do
                            let (x, y) = nodeData ! kpos
                            case aiType ai of
                                PixelA -> setC (nodeData ! kent) ctPosition $ relativePosFromPixelPos (nd ! kdim) (x, y)
                                PixelB -> setC (nodeData ! kent) ctPosition (Vec3 (-1000.0) 0 0)
                                _ -> return ()
                        else return ()
                    return ((nodeType, nodeData), (nt, nd))

                PixelB ->  do
                    let ai = nd ! kanim         -- accu, this is the previous Invader node info!
                    if aiSwapNow ai
                        then do
                            let (x, y) = nodeData ! kpos
                            case aiType ai of
                                PixelB -> setC (nodeData ! kent) ctPosition $ relativePosFromPixelPos (nd ! kdim) (x, y)
                                PixelA -> setC (nodeData ! kent) ctPosition (Vec3 (-1000.0) 0 0)
                                _ -> return ()
                        else return ()
                    return ((nodeType, nodeData), (nt, nd))

                _ -> return ((nodeType, nodeData), (nt, nd)) -- noch changes

            ) (undefined, undefined) gameData

    put (count, gameData')

    return ()

