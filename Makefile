# Makefile for festival-freebsoft-utils

# Copyright (C) 2004, 2006 Brailcom, o.p.s.
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

VERSION := 0.6
DISTDIR := festival-freebsoft-utils-$(VERSION)
TARBALL := $(DISTDIR).tar

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

distclean: clean

maintainer-clean: distclean

TAGS:

info: festival-freebsoft-utils.info
%.info: %.texi
	makeinfo $<

pdf: festival-freebsoft-utils.pdf
%.pdf: %.texi
	texi2pdf festival-freebsoft-utils.texi

dist: clean info
	mkdir $(DISTDIR)
	chmod 755 $(DISTDIR)
	install -m 644 [A-Z]* *.scm *.texi *.info* $(DISTDIR)
	tar cvf $(TARBALL) $(DISTDIR)
	gzip -9 $(TARBALL)

check:

