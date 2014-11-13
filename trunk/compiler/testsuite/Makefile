TEST = A22006B
GELA_BUILD ?= ../build
ODIR = $(GELA_BUILD)
EXE = $(patsubst %,$(ODIR)/%,$(TEST:=.exe))

ACATS = ../../../acats
ADACOMP = $(ODIR)/gela-compiler -I$(ACATS)/include/ 
VPATH=$(ACATS)/A:support

run: $(EXE)
	for J in $(EXE); do lli $$J; done | tee $(ODIR)/run.log
	diff -u expected $(ODIR)/run.log

$(ODIR)/%.ll: %.ADA
	$(ADACOMP) -I$(dir $<) $(notdir $<) > $@

$(ODIR)/%.ll: %.c
	clang -S -emit-llvm -o $@ $<

$(ODIR)/%_main.ll: main.c
	clang -S -emit-llvm -DTEST=$* -o $@ $<

$(ODIR)/%.exe: $(ODIR)/%_main.ll $(ODIR)/%.ll $(ODIR)/test_support.ll
	llvm-link -o $@ $^
