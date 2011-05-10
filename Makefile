.PHONY: all check install clean

all: tyson

tyson: tyson.hs
	ghc -Wall --make $@

check: tyson
	./tyson --runtests

install: tyson
	install tyson $(DESTDIR)/bin

clean:
	rm -f tyson *.hi *.o
