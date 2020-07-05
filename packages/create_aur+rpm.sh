#!/bin/bash
echo "RPM"
alien -r -c -v tomboy-reborn_1.0.0-1_amd64.deb
echo "ARCH"
makepkg --printsrcinfo > .SRCINFO
makepkg

