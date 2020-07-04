#!/bin/bash
echo "RPM"
alien -r -c -v tomboy-reborn_1.0.beta5-2_amd64.deb
echo "ARCH"
makepkg --printsrcinfo > .SRCINFO
makepkg

