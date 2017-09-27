BUILDDIR   ?= build
RUNHASKELL ?= runhaskell Setup --builddir=$(BUILDDIR)

prefix ?= /usr
HFLAGS := -O --enable-shared --enable-executable-dynamic --disable-library-vanilla \
	  --prefix=$(prefix) --docdir=$(prefix)/share/doc/rationalis		   \
	  --libsubdir=\$compiler/site-local/\$pkgid -f-lib-only $(HFLAGS)

all: rationalis

.PHONY: install rationalis test clean

rationalis: configure
	$(RUNHASKELL) build

configure: $(BUILDDIR)/setup-config
$(BUILDDIR)/setup-config:
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
	$(RUNHASKELL) test
