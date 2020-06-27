#!/bin/bash
echo "DEBIAN"
checkinstall --pkgname=tomboy-reborn --maintainer="\"Joan Moreau <jom@grosjo.net>\""  --requires=libcanberra-gtk-module --pkggroup=x11 --pkgversion=1.0 --pkgsource="https://github.com/grosjo/tomboy-reborn" --pkgrelease=beta4
echo "RPM"
alien -r -c -v tomboy-reborn_1.0-beta4_amd64.deb
echo "ARCH"
makepkg --printsrcinfo > .SRCINFO
makepkg

