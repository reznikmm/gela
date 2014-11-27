GELA_BUILD ?= /tmp

all:
	ADA_PROJECT_PATH=$(ADA_PROJECT_PATH):../gnat \
	gprbuild -m -j0 -p -P gnat/compiler.gpr

tests: all
	$(MAKE) -C testsuite $(GELA_BUILD)/test_support.ll