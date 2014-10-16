#!/bin/sh

xkbcomp -I${HOME}/.xkb ${HOME}/.xkb/keymap/dimir ${DISPLAY}

#xset m 2 1
#xset b 50 50 50

xset b off

xrdb -merge ${HOME}/.Xdefaults
