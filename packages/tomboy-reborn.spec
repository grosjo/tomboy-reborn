Name:           tomboy-reborn
Version:        1.0.0
Release:        1
Summary:        A drop-in, straightforward, replacement of deprecated, but extremely usefull, Gnome Tomboy

Group:          Applications/Productivity
License:        LGPL
URL:            https://github.com/grosjo/tomboy-reborn/
Source0:        https://github.com/grosjo/tomboy-reborn/blob/master/packages/tomboy-reborn-%{version}.tar.gz

BuildRequires:  lazarus, git

%description
A drop-in, straightforward, replacement of deprecated, but extremely usefull, Gnome Tomboy

%global debug_package %{nil}

%prep
%setup -q
%build
%{make_build}

%install
%{make_install} PREFIX=%{_prefix}

%files
/usr/bin/tomboy-reborn
/usr/share/applications/tomboy-reborn.desktop
/usr/share/icons/hicolor/256x256/apps/tomboy-reborn.png

%changelog
* Sat Jul 11 2020 Joan Moreau 1.0.0
  - Initial release
