all: 
#	lazbuild -B tomboy-reborn.lpi

install:
	mkdir -p $(DESTDIR)/usr/bin/
	install -Dm755 ./tomboy-reborn $(DESTDIR)/usr/bin/
	mkdir -p $(DESTDIR)/usr/share/applications/
	install -Dm644 ./tomboy-reborn.desktop $(DESTDIR)/usr/share/applications/
	mkdir -p $(DESTDIR)/usr/share/icons/hicolor/256x256/apps/
	install -Dm644 ./tomboy-reborn.png $(DESTDIR)/usr/share/icons/hicolor/256x256/apps/

clean:

distclean: clean

uninstall: 
	-rm -f $(DESTDIR)/usr/bin/tomboy-reborn
	-rm -f $(DESTDIR)/usr/share/applications/tomboy-reborn.desktop
	-rm -f $(DESTDIR)/usr/share/icons/hicolor/256x256/apps/tomboy-reborn.png

