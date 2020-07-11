#!/bin/bash
echo "Create DEBIAN package"
V=1.0.0
P=tomboy-reborn
D=${P}-${V}
rm ${P}_${V}*
tar -zxvf $D.tar.gz 
cp -a debian $D/
cd $D/
debmake
debuild 
cd ..
rm *.buildinfo
rm *.build
rm *.changes
