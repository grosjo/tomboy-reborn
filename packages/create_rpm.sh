#!/bin/bash
echo "Create RPM package"
V=1.0.0
P=tomboy-reborn
D=${P}-${V}
rm -rf ~/rpmbuild
rpmdev-setuptree
cp $D.tar.gz ~/rpmbuild/SOURCES/
cp tomboy-reborn.spec ~/rpmbuild/SPECS/
spectool -g -R ~/rpmbuild/SPECS/tomboy-reborn.spec
rpmbuild -ba ~/rpmbuild/SPECS/tomboy-reborn.spec
cp ~/rpmbuild/RPMS/x86_64/*.rpm ./
cp ~/rpmbuild/SRPMS/*.rpm ./

