{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module XMonadExt.Controller (
    changeVolume
  , changeKbdLayout
  , changeExt
  , changeExtFile
  , myKeyExtAction
  , initExtStateF
  ) where 

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig

import Control.Lens
import Data.Default
import Data.Void
import Control.Monad.State hiding (mapM, mapM_, forM, forM_)
import Control.Monad.Reader hiding (mapM, mapM_, forM, forM_)

import System.Environment
import System.FilePath
import System.IO
import Data.Foldable
import Data.List hiding (concat)
import Prelude hiding (concat)

import XMonadExt.Types (
    Volume 
  , Mute (..)
  , KbdLayout (..)

  , VolumeConfig (..)
  , HasVolumeConfig (..)
  , KbdLayoutConfig (..)
  , HasKbdLayoutConfig (..)
  , ExtConfig (..)
  , HasExtConfig (..)

  , VolumeState (..)
  , HasVolumeState (..)
  , averageVolume
  , KbdLayoutState (..)
  , HasKbdLayoutState (..)
  , ExtState (..)
  , HasExtState (..)

  , VolumeCommand (..)
  , AsVolumeCommand (..)
  , KbdLayoutCommand (..)
  , AsKbdLayoutCommand (..) 
  , ExtCommand (..)
  , AsExtCommand (..)
  ) 

import XMonadExt.Switch (
    on

  , VolumeCommandF (..)
  , _IncreaseF
  , _DecreaseF
  , _MuteF
  , _UnmuteF
  , fromVolumeCommand
  , switchVolumeCommandF

  , KbdLayoutCommandF (..)
  , _SwitchToUSF
  , _SwitchToRUF
  , fromKbdLayoutCommand
  , switchKbdLayoutCommandF

  , ExtCommandF (..)
  , _ExtVolumeCommandF
  , _ExtKbdLayoutCommandF
  , fromExtCommand
  , switchExtCommandF
  )

import XMonadExt.Default (myKeyCommand)

----------

-- use (lift $ do) in case of (ReaderT e (StateT s m)) ()
changeVolume :: (HasVolumeConfig e, MonadReader e m, HasVolumeState s, MonadState s m, MonadIO m) => VolumeCommand -> m () 
changeVolume = go . fromVolumeCommand where 
  go = switchVolumeCommandF
    & on _IncreaseF (\() -> do 
        mt <- use mute
        when (mt == SoundOn) $ do 
          maxVol <- view maxVolume
          q <- view quantVolume
          xs <- view sinks
--          lift $ do 
          volume %= ((min maxVol) . (+ q))
          v <- use volume
--            lift $ do 
          forM_ xs $ (\i -> spawn (intercalate " " ["pactl set-sink-volume", show i, show v])))
    & on _DecreaseF (\() -> do 
        mt <- use mute 
        when (mt == SoundOn) $ do 
          minVol <- view minVolume
          q <- view quantVolume
          xs <- view sinks
          volume %= ((max minVol) . (+ (-q)))
          v <- use volume
          forM_ xs $ (\i -> spawn (intercalate " " ["pactl set-sink-volume", show i, show v])))
    & on _UnmuteF (\() -> do 
         xs <- view sinks
         mute .= SoundOn
         forM_ xs $ (\i -> spawn (intercalate " " ["pactl set-sink-mute", show i, "0"])))
    & on _MuteF (\() -> do 
         xs <- view sinks
         mute .= SoundOff
         forM_ xs $ (\i -> spawn (intercalate " " ["pactl set-sink-mute", show i, "1"])))

----------

changeKbdLayout :: (HasKbdLayoutConfig e, MonadReader e m, HasKbdLayoutState s, MonadState s m, MonadIO m) => KbdLayoutCommand -> m ()
changeKbdLayout = go . fromKbdLayoutCommand where 
  go = switchKbdLayoutCommandF 
    & on _SwitchToUSF (\() -> do 
        kbdLayout .= KbdLayoutUS
        spawn (intercalate " " ["setxkbmap", "us"]))
    & on _SwitchToRUF (\() -> do 
        kbdLayout .= KbdLayoutRU 
        spawn (intercalate " " ["setxkbmap", "ru"]))
 
----------

changeExt :: (HasKbdLayoutConfig e, HasVolumeConfig e, MonadReader e m, HasVolumeState s, HasKbdLayoutState s, MonadState s m, MonadIO m) => ExtCommand -> m ()
changeExt = go . fromExtCommand where 
  go = switchExtCommandF 
    & on _ExtVolumeCommandF changeVolume    -- (\(x :: VolumeCommand) -> changeVolume x)
    & on _ExtKbdLayoutCommandF changeKbdLayout    -- (\(x :: KbdLayoutCommand) -> changeKbdLayout x)

changeExtFile :: (MonadIO m) => FilePath -> ExtCommand -> m ()
changeExtFile fp cmd = do 
  curr <- liftIO $ withFile fp ReadMode $ \h -> do 
    str <- hGetLine h
    readIO str    -- :: IO ExtState
  updt <- flip execStateT (curr :: ExtState) $ flip runReaderT (def :: ExtConfig) $ changeExt cmd 
  liftIO $ withFile fp WriteMode $ \h -> do 
    hPutStrLn h $ show updt   -- do not forget Ln

----------

--- ***
myKeyExtAction :: (MonadIO m) => FilePath -> [(String, m ())]
myKeyExtAction fp = myKeyCommand & traverse._2 %~ changeExtFile fp

-- ***
initExtStateF :: (MonadIO m) => FilePath -> m ()
initExtStateF fp = liftIO $ do 
  withFile fp WriteMode $ 
    \h -> hPutStrLn h $ show (def :: ExtState)   -- do not forget Ln

----------
