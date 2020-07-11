#!/bin/bash
echo "Create TAR package"
V=1.0.0
P=tomboy-reborn
D=${P}-${V}
rm -rf ${D}*
mkdir -p ${D}
cp ../LICENSE ./$D/
cp ../src/*.pas ../src/*.lfm ../src/*.lpr ../src/*.lpi ../src/*.res ../src/*.ico ./$D/
cp ./Makefile ./$D/
#cp ../src/tomboy-reborn ./$D/
cp ../tomboy-reborn.desktop ./$D/
cp ../tomboy-reborn.png ./$D/
tar -czf $D.tar.gz $D
rm -rf ${D}
