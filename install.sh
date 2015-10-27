#!/bin/bash

if [ ! -d ~/.xmonad ]; then
    echo "Creating directory ~/.xmonad."
    mkdir ~/.xmonad
fi

if [ -f ~/.xmonad/start-xmonad ]; then
    echo "Error: ~/.xmonad/start-xmonad already exists."
    exit 1;
fi

if [ ! -d ~/.xmonad/xmonad-ext ]; then
    echo "Creating directory ~/.xmonad/xmonad-ext."
    mkdir ~/.xmonad/xmonad-ext
fi

if [ -f ~/.xmonad/xmonad-ext/xmonad ]; then
    echo "Error: ~/.xmonad/xmonad-ext/xmonad already exists."
    exit 1;
fi

if [ -f ~/.xmonad/xmonad-ext/showVol ]; then
    echo "Error: ~/.xmonad/xmonad-ext/showVol already exists."
    exit 1;
fi

if [ -f ~/.xmonad/xmonad-ext/xmobar ]; then
    echo "Error: ~/.xmonad/xmonad-ext/xmobar already exists."
    exit 1;
fi

if [ -f ~/.xmonad/xmonad-ext/xmobar.config ]; then
    echo "Error: ~/.xmonad/xmonad-ext/xmobar.config already exists."
    exit 1;
fi

##########

cd ./.cabal-sandbox/bin

if [ ! -f `pwd`/xmonad-ext ]; then
    echo "Error: xmonad-ext is not found."
    exit 1;
fi
ln -s `pwd`/xmonad-ext ~/.xmonad/xmonad-ext/xmonad

if [ ! -f `pwd`/showVol-ext ]; then
    echo "Error: showVol-ext is not found."
    exit 1;
fi
ln -s `pwd`/showVol-ext ~/.xmonad/xmonad-ext/showVol

cd -

if [ ! -f xmobar-config/xmobar.config ]; then
    echo "Error: xmobar.config is not found."
    exit 1;
fi
ln -s `pwd`/xmobar-config/xmobar.config ~/.xmonad/xmonad-ext/xmobar.config

if [ -z "$(which xmobar)" ]; then
    echo "Error: xmobar is not found."
    exit 1;
fi
ln -s `which xmobar` ~/.xmonad/xmonad-ext/xmobar

cd ~/.xmonad

ln -s `pwd`/xmonad-ext/xmonad start-xmonad

cd -
