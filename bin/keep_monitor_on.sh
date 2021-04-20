#!/bin/sh
# Turn on Display
xset -display :0.0 dpms force on

# Disable DPMS and prevent screen from blanking
xset s off -dpms
