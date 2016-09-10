{-# LANGUAGE OverloadedStrings #-}

module Music (
        musicActorF
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


-- MUSIC ACTOR
-- -----------

type MaR = HG3D
type MaS = (Entity, Entity, Entity)

musicActorF :: Message -> ReaderStateIO MaR MaS ()
musicActorF m = do
    hg3d <- lift ask
    (music, shot, no_shot) <- get

    case m of 
        InitActor -> do
            music <- liftIO $ newE hg3d [ 
                ctSoundSource #: Music "Sounds/RMN-Music-Pack/OGG/CD 3 - Clash of Wills/3-04 Joyful Ocean.ogg" 1.0 True "Music", 
                ctPlayCmd #: Stop ] 
            no_shot <- liftIO $ newE hg3d [ 
                ctSoundSource #: Sound "Sounds/inventory_sound_effects/ring_inventory.wav" 1.0 False "Sounds",
                ctPlayCmd #: Stop ]
            shot <- liftIO $ newE hg3d [ 
                ctSoundSource #: Sound "Music/shoot.wav" 1.0 False "Sounds",
                ctPlayCmd #: Stop ]
            invader_killed <- liftIO $ newE hg3d [ 
                ctSoundSource #: Sound "Music/invaderkilled.wav" 1.0 False "Sounds",
                ctPlayCmd #: Stop ]
            put (music, shot, no_shot)
            return ()

        PlayShot -> liftIO (setC shot ctPlayCmd Play) >> return ()
        PlayNoShot -> liftIO (setC no_shot ctPlayCmd Play) >> return ()
        -- PlayExplosion -> liftIO (setC invader_killed ctPlayCmd Play) >> return ()

        StartMusic -> liftIO (setC music ctPlayCmd Play) >> return ()
        StopMusic -> liftIO (setC music ctPlayCmd Stop) >> return ()
        _ -> return ()



