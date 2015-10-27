## XMONAD-CONFIGURATION

The present repository contains my working configuration of XMonad.  

I am using it as a playground to test lenses and prisms.

## INSTALLATION

1. Install xmobar:

   ```
   cabal update  
   cabal install xmobar  
   ```
   
1. Clone xmonad-configuration from GitHub and install the package into a cabal sandbox: 

   ```
   git clone ...  
   cd xmonad-configuration  
   cabal sandbox init  
   cabal update  
   cabal install   
   ```

   Note that this will install xmonad and xmonad-contrib as dependencies. 

   If the build is successful, the directory `./.cabal-sandbox/bin` must contain, apart from the `xmonad` binary, a pair of files, `showVol-ext` and `xmonad-ext`. 

2. Create a backup of `~/.xmonad`:

   ```
   mv ~/.xmonad ~/.xmonad.orig  
   ```

3. `xmonad-ext` is a binary file which is meant to replace the original `xmonad` binary associated to `xmonad.hs` in `~/.xmonad`.  

   `showVol-ext` is an auxiliary binary "show volume" to be used by xmobar as mentioned in the configuration file `xmobar-config/xmobar.config`.  

    Create symbolic links in `~/.xmonad` running a script:

   ```
   ./install.sh
   ```

4. The link `~/.xmonad/start-xmonad` points to `~/.xmonad/xmonad-ext/xmonad`, which in turn points to the `xmonad-ext` binary in the sandbox. Edit `/usr/share/xsessions/xmonad.desktop`: 

   ```
   [Desktop Entry]  
   Encoding=UTF-8  
   Name=XMonad  
   Comment=Lightweight tiling window manager  
   Exec=~/.xmonad/start-xmonad  
   Icon=  
   Type=XSession  
   ```

Logout, select XMonad in the display manager, and re-login.  

Have fun!
