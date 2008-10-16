# Makefile for festival-freebsoft-utils

# Copyright (C) 2004, 2006, 2007, 2008 Brailcom, o.p.s.
#
# Author: Milan Zamazal <pdm@brailcom.org>
#
# COPYRIGHT NOTICE
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA.

PROJECT := festival-freebsoft-utils
VERSION := 0.10

DISTDIR := festival-freebsoft-utils-$(VERSION)
TARBALL := $(DISTDIR).tar
DOCDIR := doc

.PHONY: all install install-strip uninstall clean distclean mostlyclean \
	maintainer-clean TAGS info dvi dist check

all: info

install:

install-strip:
	$(MAKE) INSTALL_PROGRAM='$(INSTALL_PROGRAM) -s' install

uninstall:

mostlyclean:

clean: mostlyclean
	rm -rf $(DISTDIR) $(TARBALL)*
	rm -f doc/*.info doc/*.ps doc/*.pdf doc/*.html doc/*.dvi

distclean: clean

maintainer-clean: distclean

TAGS:

info: $(DOCDIR)/$(PROJECT).info
%.info: %.texi
	cd $(DOCDIR) && makeinfo $(PROJECT).texi

pdf: $(DOCDIR)/$(PROJECT).pdf
%.pdf: %.texi
	cd $(DOCDIR) && texi2pdf $(PROJECT).texi

dvi: $(DOCDIR)/$(PROJECT).dvi
%.dvi: %.texi
	cd $(DOCDIR) && texi2dvi $(PROJECT).texi

ps: $(DOCDIR)/$(PROJECT).ps
%.ps: %.texi dvi
	cd $(DOCDIR) && dvips $(PROJECT).dvi

html: $(DOCDIR)/$(PROJECT).html
%.html: %.texi
	cd $(DOCDIR) &&	\
	makeinfo --html --no-split $(PROJECT).texi

doc-all: info pdf ps dvi html

dist: clean info
	mkdir $(DISTDIR)
	chmod 755 $(DISTDIR)
	install -m 644 [A-Z]* *.scm $(DISTDIR)
	mkdir -m 755 $(DISTDIR)/doc
	install -m 644 doc/*.texi doc/*.info* $(DISTDIR)/doc/
	tar cvf $(TARBALL) $(DISTDIR)
	gzip -9 $(TARBALL)

check:

