{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module XMonadExt.Default (
    myKeyVolumeCommand
  , myKeyKbdLayoutCommand
  , myKeyCommand
  , xmonadExt
  , extStateFileName
  ) where

import qualified GHC.Generics as GHC
import Control.Lens
import Data.Default
import Data.Void

import XMonadExt.Types (
    Volume (..)
  , Mute (..)
  , KbdLayout (..)
  , VolumeConfig (..)
  , KbdLayoutConfig (..)
  , ExtConfig (..)
  , VolumeState (..)
  , averageVolume
  , KbdLayoutState (..)
  , ExtState (..)
  , VolumeCommand (..)
  , AsVolumeCommand (..)
  , KbdLayoutCommand (..)
  , AsKbdLayoutCommand (..)
  ) 

----------

instance Default VolumeConfig where 
  def = VolumeConfig { _sinks = [0]
                     , _quantVolume = 3000
                     , _maxVolume = 90000
                     , _minVolume = 0 }

instance Default KbdLayoutConfig where 
  def = KbdLayoutConfig {}

instance Default ExtConfig where 
  def = ExtConfig { _extVolumeConfig = def 
                  , _extKbdLayoutConfig = def }

----------

-- Defaults for the states are the initial values
instance Default VolumeState where 
  def = VolumeState { _volume = averageVolume (def :: ExtConfig) 
                    , _mute = SoundOn }

instance Default KbdLayoutState where 
  def = KbdLayoutState { _kbdLayout = KbdLayoutUS } 

instance Default ExtState where 
  def = ExtState { _extVolume = def
                 , _extKbdLayout = def }

----------

myKeyVolumeCommand :: (AsVolumeCommand t) => [(String, t)] 
myKeyVolumeCommand = 
  [ ( "M-.", Increase) 
  , ( "M-,", Decrease) 
  , ( "M-S-.", Unmute) 
  , ( "M-S-,", Mute) 
  ] & traverse._2 %~ review _VolumeCommand    -- embedVolumeCommand = review _VolumeCommand :: (AsVolumeCommand t) => VolumeCommand -> t

myKeyKbdLayoutCommand :: (AsKbdLayoutCommand t) => [(String, t)] 
myKeyKbdLayoutCommand = 
  [ ( "M-=", SwitchToUS)
  , ( "M-0", SwitchToRU) 
  ] & traverse._2 %~ review _KbdLayoutCommand   -- embedKbdLayoutCommand = review _KbdLayoutCommand :: (AsKbdLayoutCommand t) => KbdLayoutCommand -> t

myKeyCommand :: (AsVolumeCommand t, AsKbdLayoutCommand t) => [(String, t)]
myKeyCommand = concat
  [ myKeyVolumeCommand 
  , myKeyKbdLayoutCommand ]

----------

-- installation directory relative to $HOME
xmonadExt :: FilePath
xmonadExt = ".xmonad/xmonad-ext"   -- correlate this with the configuration file for xmobar

extStateFileName :: FilePath
extStateFileName = ".ext-state"



