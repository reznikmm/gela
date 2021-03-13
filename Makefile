# SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
#
# SPDX-License-Identifier: MIT
#

GPRBUILD_FLAGS = -p -j0
PREFIX                 ?= /usr
GPRDIR                 ?= $(PREFIX)/share/gpr
LIBDIR                 ?= $(PREFIX)/lib
INSTALL_PROJECT_DIR    ?= $(DESTDIR)$(GPRDIR)
INSTALL_INCLUDE_DIR    ?= $(DESTDIR)$(PREFIX)/include/oasis
INSTALL_LIBRARY_DIR    ?= $(DESTDIR)$(LIBDIR)
INSTALL_ALI_DIR        ?= ${INSTALL_LIBRARY_DIR}/oasis

GPRINSTALL_FLAGS = --prefix=$(PREFIX) --link-lib-subdir=$(INSTALL_LIBRARY_DIR)\
 --lib-subdir=$(INSTALL_ALI_DIR) --project-subdir=$(INSTALL_PROJECT_DIR)\
 --sources-subdir=$(INSTALL_INCLUDE_DIR)

all:
	gprbuild $(GPRBUILD_FLAGS) -P gnat/ada_larl.gpr
	# .objs/ada-larl/ada_larl source/parser/ada-lalr.ag > /tmp/aaa
	# gnatchop -w /tmp/aaa source/parser/
	gprbuild $(GPRBUILD_FLAGS) -P gnat/oasis.gpr
	gprbuild $(GPRBUILD_FLAGS) -P gnat/oasis_plain.gpr \
	  -u program-parsers-data.adb -cargs -O0
	gprbuild $(GPRBUILD_FLAGS) -P gnat/oasis_plain.gpr
	gprbuild $(GPRBUILD_FLAGS) -P gnat/examples.gpr

install:
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/oasis.gpr
	gprinstall $(GPRINSTALL_FLAGS)/plain -p -P gnat/oasis_plain.gpr
clean:
	gprclean -q -P gnat/oasis.gpr

check: DN=.objs/examples/def_name
check: all .acats
	@set -e; \
	for DIR in A; do \
	  for J in tests/def_name/acats/$$DIR/*; do \
	    UNIT=`basename $$J .txt`; echo -n $$UNIT : ;\
	    $(DN) -I.acats -Iexamples/dump_tree/ .acats/$$DIR/$$UNIT* >/tmp/new;\
	    diff -u $$J /tmp/new ; \
	  done; \
	  echo $$DIR def_name done ; \
	done

.acats:
	mkdir .acats
	curl -s -o .acats/ACATS41.ZIP https://www.ada-ru.org/files/ACATS41.ZIP
	unzip -q -aa .acats/ACATS41.ZIP -d .acats
	gnatchop -q .acats/SUPPORT/REPORT.A .acats
	gnatchop -q .acats/SUPPORT/MACROSUB.ADA /tmp/
	(cd /tmp; gprbuild macrosub.adb)
	find $(PWD)/.acats -name *.TST > .acats/SUPPORT/TSTTESTS.DAT
	(cd .acats/SUPPORT/; /tmp/macrosub)
	rm -v -f .acats/ACATS41.ZIP `cat .acats/SUPPORT/TSTTESTS.DAT`
