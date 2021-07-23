#!/bin/sh
feh --bg-fill ~/Pictures/rocket.png

picom --daemon
exec xmonad
