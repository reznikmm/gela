export GELA_BUILD=$(abspath build)
AG_DRIVER=$(GELA_BUILD)/ag/ag_driver
YACC_DRIVER=$(GELA_BUILD)/ag/yacc_driver
UAFLEX=$(GELA_BUILD)/tools/uaflex-driver
AST=$(GELA_BUILD)/ada-ast.ag
AST_STAMP=$(GELA_BUILD)/.stamp-ast
SYNTAX=src/parser/ada-lalr.ag
BUILD_CHOP=$(GELA_BUILD)/chop
BUILD_NODE=$(GELA_BUILD)/node
LEXER=$(abspath src/lexer/ada.l)
LEXER_STAMP=$(GELA_BUILD)/.stamp-ada.l
PARSER=$(GELA_BUILD)/.stamp-ada.p

all: asis tests
	echo gprbuild -m -j0 -p -P gnat/gela_debug.gpr

asis : $(AST_STAMP) $(LEXER_STAMP) $(PARSER)
	gprbuild -m -j0 -p -P gnat/gela_build.gpr

$(AST_STAMP): $(AG_DRIVER) $(AST)
	$(AG_DRIVER) $(AST) > $(AST_STAMP)
	-mkdir $(BUILD_CHOP)
	-mkdir $(BUILD_NODE)
	gnatchop -w $(AST_STAMP) $(BUILD_CHOP)

$(AST): src/ag/*.ag src/ag/main.ag.pp
	cpp -P src/ag/main.ag.pp $(AST)

$(LEXER_STAMP): $(UAFLEX) $(LEXER)
	cd $(BUILD_CHOP); $(UAFLEX) --types Gela.Scanner_Types \
            --handler Gela.Scanner_Handlers --scanner Gela.Scanners \
            --tokens Gela.Lexical_Types $(LEXER)
	touch $(LEXER_STAMP)

$(PARSER): $(YACC_DRIVER) $(SYNTAX)
	$(YACC_DRIVER) $(SYNTAX) > $(PARSER)
	gnatchop -w $(PARSER) $(BUILD_CHOP)

$(AG_DRIVER) $(YACC_DRIVER):
	gprbuild -m -j0 -p -P gnat/gela_ag.gpr

$(UAFLEX):
	gprbuild -m -j0 -p -P gnat/gela_uaflex2.gpr

tests: asis
	gprbuild -m -j0 -p -P gnat/gela_tester.gpr
	gprbuild -m -j0 -p -P gnat/gela_grammars_tests.gpr
	gprbuild -m -j0 -p -P gnat/gela_tests.gpr
	gprbuild -m -j0 -p -P gnat/gela_asis_tests.gpr
ifneq ($(realpath compiler),)
	-$(MAKE) -C compiler tests
endif
ifneq ($(realpath tests/asis/asis2xml.gpl),)
	ADA_PROJECT_PATH=$(ADA_PROJECT_PATH):gnat \
	gprbuild -m -j0 -p -P tests/asis/asis2xml.gpl/gela2xml.gpr
endif

check:
	GELA_INCLUDE_PATH=$(realpath src/adalib/) \
	$(GELA_BUILD)/gela-test_driver2
