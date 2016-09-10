{-# LANGUAGE OverloadedStrings #-}

module Input where

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

-- KEY INPUT ACTOR
-- ---------------

type KiaR = (HG3D, Actor)
type KiaS = (Var [KeyEvent], [T.Text])

keyInputActorF :: Message -> ReaderStateIO KiaR KiaS ()
keyInputActorF msg = do

    (hg3d, screenA) <- lift ask
    (keyevts, keysdown) <- get

    case msg of 

        InitActor -> do
            keyevts' <- liftIO $ makeVar []
            let handleKey k = updateVar keyevts' (\l -> (k : l, ()))
            ieh <- liftIO $ newE hg3d [ctInputEventHandler #: DefaultEventHandler, ctKeyEvent #: NoKeyEvent]
            liftIO $ registerCallback hg3d ieh ctKeyEvent handleKey
            put (keyevts', keysdown) >> return ()

        PollKeys -> do
            keys <- liftIO $ updateVar keyevts (\l -> ([], l))
            keysdown' <- foldM (\kd k -> do
                    case k of
                        KeyDown _ _ k -> do
                            let kd' = if not (k `elem` kd) then (k : kd) else kd
                            return kd'

                        KeyUp _ _ k -> do
                            let kd' = filter (\k' -> k' /= k) kd
                            liftIO $ sendMsg screenA (SingleKey k)
                            return kd'
                ) keysdown (reverse keys)

            if length keysdown' > 0 
                then liftIO $ sendMsg screenA (KeysPressed keysdown')
                else return ()

            put (keyevts, keysdown') >> return ()


