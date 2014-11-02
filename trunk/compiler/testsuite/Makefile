TEST= A22006B
ODIR = /tmp
EXE = $(patsubst %,$(ODIR)/%,$(TEST:=.exe))

VPATH=acats/A:support

run: $(EXE)
	for J in $(EXE); do lli $$J; done | tee $(ODIR)/run.log
	diff -u expected $(ODIR)/run.log

$(ODIR)/%.ll: %.ADA
	echo ADACOMP -o $@ $<
	cat support/a22006b.s > $@

$(ODIR)/%.ll: %.c
	clang -S -emit-llvm -o $@ $<

$(ODIR)/%_main.ll: main.c
	clang -S -emit-llvm -DTEST=$* -o $@ $<

$(ODIR)/%.exe: $(ODIR)/%_main.ll $(ODIR)/%.ll $(ODIR)/test_support.ll
	llvm-link -o $@ $^
