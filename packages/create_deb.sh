#!/bin/bash
echo "Create DEBIAN package"
V=1.0.0
P=tomboy-reborn
D=${P}-${V}
rm -rf ${D}*
rm ${P}_${V}*
mkdir -p ${D}/src
cp ../LICENSE ./$D/
# cp ../src/*.pas ../src/*.lfm ../src/*.lpr ../src/*.lpi ../src/*.res ../src/*.ico ./$D/src/
cp ../src/Makefile ./$D/
cp ../src/tomboy-reborn ./$D/
cp ../tomboy-reborn.desktop ./$D/
cp ../tomboy-reborn.png ./$D/
tar -czvf $D.tar.gz $D
cp -a debian $D/
cd $D/
#debmake
#debuild -d

