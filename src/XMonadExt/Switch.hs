{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module XMonadExt.Switch (
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
  ) where

import Control.Lens
import qualified GHC.Generics as GHC
import Data.Void
import qualified Control.Category.Cartesian as CCC

import XMonadExt.Types (
    VolumeCommand (..)
  , KbdLayoutCommand (..)
  , ExtCommand (..)    
  )

----------

-- See the package 'total' on hackage.
on :: ((a1 -> Either a1 Void) -> t -> Either a b) -> (a -> c) -> (b -> c) -> t -> c    --- Void <---> b1 is needed to avoid multiple same matches
-- on p f g x = either f g (p Left x)
on p f g x = (CCC.|||) f g (p Left x)

----------

data VolumeCommandF e0 e1 e2 e3 = IncreaseF e0 | DecreaseF e1 | UnmuteF e2 | MuteF e3
  deriving (Show, Read, Eq, GHC.Generic)

makePrisms ''VolumeCommandF
-- makeClassyPrisms ''VolumeCommandF

fromVolumeCommand :: VolumeCommand -> VolumeCommandF () () () ()
fromVolumeCommand = \case
  Increase -> IncreaseF ()
  Decrease -> DecreaseF ()
  Unmute -> UnmuteF ()
  Mute -> MuteF ()

switchVolumeCommandF :: VolumeCommandF Void Void Void Void -> a
switchVolumeCommandF = \case 
  IncreaseF x -> absurd x
  DecreaseF x -> absurd x 
  UnmuteF x -> absurd x
  MuteF x -> absurd x

----------

data KbdLayoutCommandF f0 f1 = SwitchToUSF f0 | SwitchToRUF f1
  deriving (Show, Read, Eq, GHC.Generic)

makePrisms ''KbdLayoutCommandF
-- makeClassyPrisms ''KbdLayoutCommandF

fromKbdLayoutCommand :: KbdLayoutCommand -> KbdLayoutCommandF () ()
fromKbdLayoutCommand = \case 
  SwitchToUS -> SwitchToUSF ()
  SwitchToRU -> SwitchToRUF ()  

switchKbdLayoutCommandF :: KbdLayoutCommandF Void Void -> a
switchKbdLayoutCommandF = \case 
  SwitchToUSF x -> absurd x
  SwitchToRUF x -> absurd x

----------

data ExtCommandF h0 h1 = ExtVolumeCommandF h0 | ExtKbdLayoutCommandF h1 
  deriving (Show, Read, Eq, GHC.Generic)

makePrisms ''ExtCommandF
-- makeClassyPrisms ''ExtCommandF

fromExtCommand :: ExtCommand -> ExtCommandF VolumeCommand KbdLayoutCommand
fromExtCommand = \case
  ExtVolumeCommand x -> ExtVolumeCommandF x   -- ExtCommand x y -> ExtCommandF (x, y)   -- curry/ uncurry   -- check this
  ExtKbdLayoutCommand x -> ExtKbdLayoutCommandF x

switchExtCommandF :: ExtCommandF Void Void -> a
switchExtCommandF = \case 
  ExtVolumeCommandF x -> absurd x
  ExtKbdLayoutCommandF x -> absurd x

----------
