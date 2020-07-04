all: 
#	lazbuild -B tomboy-reborn.lpi

install:
	$(INSTALL_PROGRAM) ./tomboy-reborn $(DESTDIR)/bin/
	$(INSTALL_DATA) ./tomboy-reborn.desktop $(DESTDIR)/share/applications/
	$(INSTALL_DATA) ./tomboy-reborn.png $(DESTDIR)/share/icons/hicolor/256x256/apps/

clean:

distclean: clean

uninstall: 
	-rm -f $(DESTDIR)/bin/tomboy-reborn
	-rm -f $(DESTDIR)/share/applications/tomboy-reborn.desktop
	-rm -f $(DESTDIR)/share/icons/hicolor/256x256/apps/tomboy-reborn.png

