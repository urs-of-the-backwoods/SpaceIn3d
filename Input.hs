{-# LANGUAGE OverloadedStrings #-}

module Input (
        newKeyActor
    ) where

import HGamer3D

import Control.Monad
import qualified Data.Text as T
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data
import Actor

-- KEY INPUT ACTOR
-- ---------------
-- single keypresses are directly send to screenA
-- polling checks in intervals on status of pressed keys, to be sent to screenA as a list

type KiaR = Actor
type KiaS = (Var [KeyEvent], [T.Text])

handleKey :: Actor -> Var [KeyEvent] -> KeyEvent -> IO ()
handleKey switchA kevts evt = do
    case evt of
        KeyDownEvent (KeyData _ _ k) -> sendMsg switchA (SingleKey k)
        _ -> return ()
    updateVar kevts (\l -> (l ++ [evt], ()))


newKeyActor :: HG3D -> Actor -> IO Actor
newKeyActor hg3d switchA = do
    keyevts <- makeVar []
    ieh <- newE hg3d [ctInputEventHandler #: DefaultEventHandler, ctKeyEvent #: NoKeyEvent]
    registerCallback hg3d ieh ctKeyEvent (handleKey switchA keyevts)

    actor <- newActor
    runActor actor keyInputActorF switchA (keyevts, [])
    return actor

keyInputActorF :: Actor -> Message -> ReaderStateIO KiaR KiaS ()
keyInputActorF keyA msg = do

    switchA <- lift ask
    (keyevts, keysdown) <- get

    case msg of 

        PollKeys -> do
            keys <- liftIO $ updateVar keyevts (\l -> ([], l))
            keysdown' <- foldM (\kd k -> do
                    case k of
                        KeyDownEvent (KeyData _ _ k) -> do
                            let kd' = if not (k `elem` kd) then (k : kd) else kd
                            return kd'

                        KeyUpEvent (KeyData _ _ k) -> do
                            let kd' = filter (\k' -> k' /= k) kd
                            return kd'
                ) keysdown (reverse keys)

            if length keysdown' > 0 
                then liftIO $ sendMsg switchA (KeysPressed keysdown')
                else return ()

            put (keyevts, keysdown') >> return ()


