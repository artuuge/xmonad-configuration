{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module XMonadExt.Types (
    Volume (..)
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
  ) where

import qualified GHC.Generics as GHC
import Control.Lens
import Data.Default
import Data.Void

----------

type Volume = Int

data Mute = SoundOff | SoundOn 
  deriving (Eq, Show, Read, Enum, GHC.Generic)

data KbdLayout = KbdLayoutUS | KbdLayoutRU 
  deriving (Show, Read, Eq, Enum, GHC.Generic)

----------

data VolumeConfig = 
  VolumeConfig { _sinks :: [Int]
               , _quantVolume :: Volume 
               , _maxVolume :: Volume 
               , _minVolume :: Volume }
  deriving (Show, Read, Eq, GHC.Generic)

makeClassy ''VolumeConfig

----------

data KbdLayoutConfig = KbdLayoutConfig {}
  deriving (Show, Read, Eq, GHC.Generic)

makeClassy ''KbdLayoutConfig

----------

data ExtConfig = ExtConfig { _extVolumeConfig :: VolumeConfig 
                           , _extKbdLayoutConfig :: KbdLayoutConfig }
  deriving (Show, Read, Eq, GHC.Generic) 

-- makeLenses ''ExtConfig
makeClassy ''ExtConfig

instance HasVolumeConfig ExtConfig where 
  volumeConfig = extVolumeConfig

instance HasKbdLayoutConfig ExtConfig where 
  kbdLayoutConfig = extKbdLayoutConfig

----------

data VolumeState = VolumeState { _volume :: Volume
                               , _mute :: Mute } 
  deriving (Show, Read, Eq, GHC.Generic)
-- instance Generic VolumeState

makeClassy ''VolumeState

averageVolume :: (HasVolumeConfig e) => e -> Volume
averageVolume e = ((e ^. maxVolume) + (e ^. minVolume)) `div` 2

----------

data KbdLayoutState = KbdLayoutState { _kbdLayout :: KbdLayout } 
  deriving (Show, Read, Eq, GHC.Generic)

makeClassy ''KbdLayoutState
-- toKbdLayoutState = view kbdLayoutState :: (HasKbdLayoutState t) => t -> KbdLayoutState

----------

data ExtState = ExtState { _extVolume :: VolumeState
                         , _extKbdLayout :: KbdLayoutState }
  deriving (Show, Read, Eq, GHC.Generic) 
-- instance Generic ExtState
-- makeLenses ''ExtState 
makeClassy ''ExtState 

instance HasVolumeState ExtState where 
  volumeState = extVolume

instance HasKbdLayoutState ExtState where 
  kbdLayoutState = extKbdLayout

----------


data VolumeCommand = Increase | Decrease | Unmute | Mute
  deriving (Show, Read, Eq, Enum, GHC.Generic)

makeClassyPrisms ''VolumeCommand
-- embedVolumeCommand = review _VolumeCommand :: (AsVolumeCommand t) => VolumeCommand -> t

----------

data KbdLayoutCommand = SwitchToUS | SwitchToRU
  deriving (Show, Read, Eq, Enum, GHC.Generic)

makeClassyPrisms ''KbdLayoutCommand
-- embedKbdLayoutCommand = review _KbdLayoutCommand :: (AsKbdLayoutCommand t) => KbdLayoutCommand -> t

----------

data ExtCommand = ExtVolumeCommand VolumeCommand | ExtKbdLayoutCommand KbdLayoutCommand 
  deriving (Show, Read, Eq, GHC.Generic)

-- makePrisms ''ExtCommand
makeClassyPrisms ''ExtCommand

instance AsVolumeCommand ExtCommand where 
  _VolumeCommand = _ExtVolumeCommand

instance AsKbdLayoutCommand ExtCommand where 
  _KbdLayoutCommand = _ExtKbdLayoutCommand

----------
