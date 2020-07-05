#!/bin/bash
echo "Create DEBIAN package"
V=1.0.0
P=tomboy-reborn
D=${P}-${V}
rm -rf ${D}*
rm ${P}_${V}*
mkdir -p ${D}
cp ../LICENSE ./$D/
cp ../src/*.pas ../src/*.lfm ../src/*.lpr ../src/*.lpi ../src/*.res ../src/*.ico ./$D/
cp ./Makefile ./$D/
#cp ../src/tomboy-reborn ./$D/
cp ../tomboy-reborn.desktop ./$D/
cp ../tomboy-reborn.png ./$D/
tar -czvf $D.tar.gz $D
cp -a debian $D/
cd $D/
debmake
debuild 
cd ..
rm -rf ${D}*
rm *.buildinfo
rm *.build
rm *.gz
rm *.changes
alien -r -c -v tomboy-reborn_${V}-1_amd64.deb
