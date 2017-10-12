BUILDDIR   ?= dist
RUNHASKELL ?= runhaskell Setup --builddir=$(BUILDDIR)

prefix ?= /usr
HFLAGS := -O --enable-shared --enable-executable-dynamic --disable-library-vanilla \
	  --prefix=$(prefix) --docdir=$(prefix)/share/doc/rationalis		   \
	  --libsubdir=\$compiler/site-local/\$pkgid -f-lib-only --enable-tests     \
	  $(HFLAGS)

config = $(BUILDDIR)/setup-config
bindir = $(BUILDDIR)/build/
executables = rationalis

.PHONY: rationalis pbz-fetcher configure test clean install

rationalis_files = src/Main.hs src/Commands.hs src/Argparse.hs src/Transformations.hs \
		   src/Transaction.hs src/Config.hs src/Lib.hs src/Rules.hs

pbz_files = src/PBZ.hs src/Lib.hs src/Transaction.hs

all: rationalis pbz-fetcher

rationalis: $(bindir)/rationalis/rationalis
pbz-fetcher: $(bindir)/pbz-fetcher/pbz-fetcher

$(bindir)/rationalis/rationalis: $(config) $(rationalis_files)
	$(RUNHASKELL) build exe:rationalis

$(bindir)/pbz-fetcher/pbz-fetcher: $(config) $(pbz_files)
	$(RUNHASKELL) build exe:pbz-fetcher

configure: $(config)
$(config):
	$(RUNHASKELL) configure $(HFLAGS)

install: rationalis
	$(RUNHASKELL) copy --destdir=$(DESTDIR)
	install -Dm644 doc/man/rationalis.1 $(DESTDIR)$(prefix)/share/man/man1/rationalis.1
	install -Dm644 doc/man/rationalis.1 $(DESTDIR)$(prefix)/share/man/man1/rationalis.1
	install -Dm644 doc/man/rationalis.5 $(DESTDIR)$(prefix)/share/man/man5/rationalis.5
	install -Dm644 doc/man/rationalis.rules.5 \
	    $(DESTDIR)$(prefix)/share/man/man5/rationalis.rules.5
	-gzip -f --best $(DESTDIR)$(prefix)/share/man/man1/rationalis.1
	-gzip -f --best $(DESTDIR)$(prefix)/share/man/man5/rationalis.5
	-gzip -f --best $(DESTDIR)$(prefix)/share/man/man5/rationalis.rules.5
	install -Dm644 contrib/completion/zsh/_rationalis \
	    $(DESTDIR)$(prefix)/share/zsh/site-functions/_rationalis
	install -Dm644 contrib/completion/bash/rationalis \
	    $(DESTDIR)$(prefix)/share/bash-completion/completions/rationalis

clean:
	$(RUNHASKELL) clean

test:
	$(RUNHASKELL) build test:rationalis-test
	$(RUNHASKELL) test
