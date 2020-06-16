#!/bin/bash
echo "DEBIAN"
checkinstall --pkgname=tomboy-reborn --maintainer="\"Joan Moreau <jom@grosjo.net>\""  --requires=libcanberra-gtk-module --pkggroup=x11 --pkgversion=1.0 --pkgsource="https://github.com/grosjo/tomboy-reborn" --pkgrelease=beta2
echo "RPM"
alien -r -c -v tomboy-reborn_1.0-beta2_amd64.deb
echo "ARCH"
debtap tomboy-reborn_1.0-beta2_amd64.deb

