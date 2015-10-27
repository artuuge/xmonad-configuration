-- module ShowVol where

import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO
import Control.Lens

import XMonadExt.Types (
    VolumeState
  , ExtState
  , HasVolumeState (
        volumeState
      , volume
      )
  )

import XMonadExt.Default (
    extStateFileName
  , xmonadExt
  )

----------

getVolumeState :: FilePath -> IO VolumeState
getVolumeState fp = withFile fp ReadMode $ \h -> do 
  str <- hGetLine h
  curr <- readIO str :: IO ExtState
  return $ curr ^. volumeState

repVolumeState :: (HasVolumeState a) => a -> String
repVolumeState a = 
  let v = a ^. volume
      m = v `div` 9000 
      n = (v `mod` 9000) `div` 3000 
      xs = [' ', '.', ':']
   in case (m, n) of 
        (0, 0) -> "[" ++ (replicate 10 ' ') ++ "]"
        (10, 0) -> "[" ++ (replicate 10 '|') ++ "]"
        _ -> "[" ++ (replicate m '|') ++ [(xs !! n)] ++ (replicate (9 - m) ' ') ++ "]"

----------

main = do 
  mh <- lookupEnv "HOME"
  let homeDir = maybe "" id $ mh
      installDir = homeDir </> xmonadExt   -- "~/.xmonad/xmonad-ext"
      extStateFile = installDir </> extStateFileName    -- "~/.xmonad/xmonad-ext/.ext-state"
  putStr . repVolumeState =<< getVolumeState extStateFile
  hFlush stdout

----------
