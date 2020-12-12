#!/bin/sh

cursor_theme=$(gsettings get org.gnome.desktop.interface cursor-theme | sed "s/'//g")
cursor_size=$(gsettings get org.gnome.desktop.interface cursor-size)

swaymsg seat seat0 $cursor_theme $cursor_size
