#!/bin/bash
echo "ARCH"
makepkg --printsrcinfo > .SRCINFO
makepkg

