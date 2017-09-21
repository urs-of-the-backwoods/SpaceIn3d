{-# LANGUAGE OverloadedStrings #-}

module Status where

import HGamer3D

import qualified Data.Traversable as Tr
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Text as T

import Data
import Actor

-- STATUS BAR ACTOR
-- ----------------

type SbaR = HG3D
type SbaS = (Int, Entity, Entity, Entity)

newStatusBarActor :: HG3D -> Int -> T.Text -> T.Text -> IO Actor
newStatusBarActor hg3d count name status = do

    eL <- liftIO $ newE hg3d [
        ctStaticText #: ("hero: " `T.append` name),
        ctScreenRect #: ScreenRect 10 10 100 25
        ]

    eM <- liftIO $ newE hg3d [
        ctStaticText #: (T.pack ("count: " ++ (show count))),
        ctScreenRect #: ScreenRect 350 10 100 25
        ]

    eR <- liftIO $ newE hg3d [
        ctStaticText #: status,
        ctScreenRect #: ScreenRect 590 10 100 25
        ]

    actor <- newActor
    runActor actor statusBarActorF hg3d (count, eL, eM, eR)
    return actor


statusBarActorF :: Actor -> Message -> ReaderStateIO SbaR SbaS ()
statusBarActorF statusA msg = do

    hg3d <- lift ask
    (count, textLeft, textMiddle, textRight) <- get

    let setY y' e = do
            r@(ScreenRect x y w h) <- readC e ctScreenRect
            setC e ctScreenRect (ScreenRect x y' w h)
            return ()

    case msg of

        DisplayStatus -> liftIO (mapM (\e -> setY 10 e) [textLeft, textMiddle, textRight]) >> return ()
        HideStatus -> liftIO (mapM (\e -> setY (-1000) e) [textLeft, textMiddle, textRight]) >> return ()
        AddCount count' -> put (count + count', textLeft, textMiddle, textRight) >> liftIO (setC textMiddle ctStaticText (T.pack ("count: " ++ (show (count + count'))))) >> return ()
        SetMode mode -> liftIO (setC textRight ctStaticText mode)  >> return ()
        _ -> return ()


