{-# LANGUAGE OverloadedStrings #-}

module Status where

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

-- STATUS BAR ACTOR
-- ----------------

type SbaR = HG3D
type SbaS = (Entity, Entity, Entity)

statusBarActorF :: Message -> ReaderStateIO SbaR SbaS ()
statusBarActorF msg = do

    hg3d <- lift ask
    (textLeft, textMiddle, textRight) <- get

    let setY y' e = do
            r@(Rectangle x y w h) <- readC e ctScreenRect
            setC e ctScreenRect (Rectangle x y' w h)
            return ()

    case msg of

        InitActor -> do
            eL <- liftIO $ newE hg3d [
                ctText #: "hero: ",
                ctScreenRect #: Rectangle 10 (-1000) 100 25
                ]

            eM <- liftIO $ newE hg3d [
                ctText #: "count: ",
                ctScreenRect #: Rectangle 350 (-1000) 100 25
                ]

            eR <- liftIO $ newE hg3d [
                ctText #: "playing",
                ctScreenRect #: Rectangle 590 (-1000) 100 25
                ]

            put (eL, eM, eR)
            return ()

        DisplayStatus -> liftIO (mapM (\e -> setY 10 e) [textLeft, textMiddle, textRight]) >> return ()
        HideStatus -> liftIO (mapM (\e -> setY (-1000) e) [textLeft, textMiddle, textRight]) >> return ()
        SetName name -> liftIO (setC textLeft ctText ("hero: " `T.append` name)) >> return ()
        SetCount count -> liftIO (setC textMiddle ctText (T.pack ("count: " ++ (show count)))) >> return ()
        SetMode mode -> liftIO (setC textRight ctText mode)  >> return ()

        _ -> return ()


