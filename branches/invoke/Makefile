GELA_BUILD=/tmp/gela
AG_DRIVER=$(GELA_BUILD)/ag/ag_driver
YACC_DRIVER=$(GELA_BUILD)/ag/yacc_driver
UAFLEX=$(GELA_BUILD)/tools/uaflex-driver
AST=src/asis/context/ada-ast.ag
AST_STAMP=$(GELA_BUILD)/.stamp-ast
SYNTAX=src/asis/sourcer/ada-lalr.ag
BUILD_CHOP=$(GELA_BUILD)/chop
BUILD_NODE=$(GELA_BUILD)/node
LEXER=$(abspath src/asis/sourcer/ada.l)
LEXER_STAMP=$(GELA_BUILD)/.stamp-ada.l
PARSER=$(BUILD_CHOP)/gela-mutables-lalr_parsers-on_reduce.adb

all: asis
	gprbuild -m -j0 -p -P gnat/gela_debug.gpr

asis : $(AST_STAMP) $(LEXER_STAMP) $(PARSER)
	gprbuild -m -j0 -p -P gnat/gela_asis.gpr

$(AST_STAMP): $(AG_DRIVER) $(AST)
	$(AG_DRIVER) $(AST) > $(AST_STAMP)
	-mkdir $(BUILD_CHOP)
	-mkdir $(BUILD_NODE)
	gnatchop -w $(AST_STAMP) $(BUILD_CHOP)
	mv $(BUILD_CHOP)/gela-nodes* $(BUILD_NODE)

$(LEXER_STAMP): $(UAFLEX) $(LEXER)
	cd $(BUILD_CHOP); $(UAFLEX) --types Gela.Lexical.Types \
	    --handler Gela.Lexical.Handlers --scanner Gela.Lexical.Scanners \
	    --tokens Gela.Lexical.Tokens $(LEXER)
	touch $(LEXER_STAMP)

$(PARSER): $(YACC_DRIVER) $(SYNTAX)
	$(YACC_DRIVER) $(SYNTAX) > $(PARSER)

$(AG_DRIVER) $(YACC_DRIVER):
	gprbuild -m -j0 -p -P gnat/gela_ag.gpr

$(UAFLEX):
	gprbuild -m -j0 -p -P gnat/gela_uaflex2.gpr
