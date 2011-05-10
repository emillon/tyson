.PHONY: all install clean

all: tyson

tyson: tyson.hs
	ghc -Wall --make $@

install: tyson
	install tyson $(DESTDIR)/bin

clean:
	rm -f tyson *.hi *.o
