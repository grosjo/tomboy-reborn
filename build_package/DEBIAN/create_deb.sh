#!/bin/bash
checkinstall --pkgname=tomboy-reborn --maintainer="\"Joan Moreau <jom@grosjo.net>\""  --requires=libcanberra-gtk-module --pkggroup=x11 --pkgversion=1.0 --pkgsource="https://github.com/grosjo/tomboy-reborn" --pkgrelease=beta1
alien -r -c -v tomboy-reborn_1.0-beta1_amd64.deb

