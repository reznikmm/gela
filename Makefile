GPRBUILD_FLAGS = -p -j0
PREFIX                 ?= /usr
GPRDIR                 ?= $(PREFIX)/share/gpr
LIBDIR                 ?= $(PREFIX)/lib
INSTALL_PROJECT_DIR    ?= $(DESTDIR)$(GPRDIR)
INSTALL_INCLUDE_DIR    ?= $(DESTDIR)$(PREFIX)/include/oasis
INSTALL_LIBRARY_DIR    ?= $(DESTDIR)$(LIBDIR)
INSTALL_ALI_DIR        ?= ${INSTALL_LIBRARY_DIR}/oasis

GPRINSTALL_FLAGS = --prefix=$(PREFIX) --sources-subdir=$(INSTALL_INCLUDE_DIR)\
 --lib-subdir=$(INSTALL_ALI_DIR) --project-subdir=$(INSTALL_PROJECT_DIR)\
--link-lib-subdir=$(INSTALL_LIBRARY_DIR)

all:
	gprbuild $(GPRBUILD_FLAGS) -P gnat/ada_larl.gpr
	# .objs/ada-larl/ada_larl source/parser/ada-lalr.ag > /tmp/aaa
	# gnatchop -w /tmp/aaa source/parser/
	gprbuild $(GPRBUILD_FLAGS) -P gnat/oasis.gpr

install:
	gprinstall $(GPRINSTALL_FLAGS) -p -P gnat/oasis.gpr
clean:
	gprclean -q -P gnat/oasis.gpr

