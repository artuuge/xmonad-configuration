-- module XMonadExt where

import System.FilePath ((</>))
import System.Environment (lookupEnv)
import Data.List (intercalate) -- hiding (concat)

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig

import XMonadExt.Controller (
    initExtStateF
  , myKeyExtAction
  )

import XMonadExt.Default (
    extStateFileName
  , xmonadExt
  )

----------

main = do 
  mh <- lookupEnv "HOME"
  let homeDir = maybe "" id $ mh
      installDir = homeDir </> xmonadExt   -- "~/.xmonad/xmonad-ext"
      extStateFile = installDir </> extStateFileName    -- "~/.xmonad/xmonad-ext/.ext-state"

  initExtStateF extStateFile    -- initialize the file containing the ExtState

  let myBar = intercalate " " $ fmap (installDir </>) ["xmobar", "xmobar.config"]
      myPP = xmobarPP 
      toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)        
      myConfig = defaultConfig
                   { modMask = mod4Mask 
                   , terminal = "terminator" 
                   } `additionalKeysP` (myKeyExtAction extStateFile)
  
  xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

----------
