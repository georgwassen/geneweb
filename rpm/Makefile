# $Id: Makefile,v 3.3.2.1 2000-04-08 22:05:57 ddr Exp $

VERSION=3.03
RELEASE=2
REDHAT=/usr/src/redhat

all:
	@echo "make install (as root)"

install:
	sed -e "s/VERSION/$(VERSION)/" -e "s/RELEASE/$(RELEASE)/" geneweb.spec > $(REDHAT)/SPECS/geneweb-$(VERSION).spec
	cp geneweb-initrc.sh $(REDHAT)/SOURCES/geneweb-initrc-$(VERSION).sh
