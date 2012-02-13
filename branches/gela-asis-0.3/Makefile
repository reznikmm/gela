##############################################################################
##                           G E L A   A S I S                              ##
##       ASIS implementation for Gela project, a portable Ada compiler      ##
##                     http://www.ten15.org/wiki/Ada                        ##
##                     # # # # # # # # # # # # # # #                        ##
##############################################################################
## Makefile has following targets                                           ##
##   all (default) - build gela asis library                                ##
##   install       - build and install gela asis library into ${PREFIX}     ##
##   generate      - generate all Ada files to build gela asis library      ##
##############################################################################

# Configuration variables:                 #################################
# Place to install
PREFIX           ?= ${HOME}/gela-asis
# Place to write files during building:
BUILD            ?= .build
# Place to write library and .ali files:
OBJ_DIR          ?= ${BUILD}/obj
# Compilation flags:
FLAGS            ?= -g -m
# If you like to use your own ayacc/afles uncomment next three lines:
# BIN_AYACC   ?= /path/to/where/it/is/ayacc
# EXTRA_BUILD ?=
# End of configuration variables.          #################################

# redistribution from SVN settings
SVN_PATH := ${HOME}/cvs_root/TenDRA/trunk/tendra
SVN_DIRS := src/lib/libada/spec src/producers/ada src/utilities/xml2ayacc
EXCLUDE := --exclude .svn --exclude Makefile --exclude tdfada
UG_URL := http://www.ten15.org/wiki/gela_asis_ug?format=txt

# Shortcuts
LIB_ASIS     := ${OBJ_DIR}/libasis.a

L            := ${BUILD}/lexer
P            := ${BUILD}/parser

WORK_DIRS    := ${BUILD} ${BUILD}/ada ${BUILD}/xml2ayacc ${L} ${P} \
	        ${BUILD}/asis ${BUILD}/uaflex ${BUILD}/ayacc ${OBJ_DIR} 

BUFFER_SRC   := ${BUILD}/ada/gela-source_buffers-current.ads
XML_TO_Y     := ${BUILD}/xml2ayacc/xml_to_y
XML_TO_Y_SRC := src/utilities/xml2ayacc/*.ad[sb]
A            := src/producers/ada

LIBGELA_SRC  := ${A}/libgela/*.ad[sb]
ENCODING_SRC := ${A}/encoding/*.ad[sb] ${A}/encoding/auto/*.ad[sb]
XML_IO_SRC   := ${A}/xml_io/*.ad[sb]
XASIS_SRC    := ${A}/xasis/*.ad[sb]
ASIS_SRC     := ${A}/asis/*.ad[sb]

PL           := ${BUILD}/ada/asis-gela-scanner_tables
LEXER_SRC    := ${PL}.adb ${PL}.ads

UAFLEX_BIN   := ${BUILD}/uaflex/uaflex
UAFLEX_SRC   := src/utilities/uaflex/*.ad[sb]

PP           := ${BUILD}/ada/asis-gela-parser
PARSER_SRC   := ${PP}.ads ${PP}.adb ${PP}-goto_table.ads \
                ${PP}-shift_reduce.ads ${PP}-tokens.ads

ASIS_SRC     := ${BUILD}/ada/asis.adb ${BUILD}/ada/asis.ads

XML   := ${A}/xml
UAFLEX := ${XML}/uaflex
AYACC := ${XML}/ayacc
ASIS  := ${XML}/asis

BIN_INSTALL  ?= install
BIN_XSLTPROC ?= xsltproc
BIN_SED      ?= sed
BIN_ECHO     ?= echo
BIN_CAT      ?= cat
BIN_CP       ?= cp
BIN_GNATCHOP ?= gnatchop
BIN_GNATMAKE ?= gnatmake
BIN_AYACC    ?= ${BUILD}/ayacc/ayacc

EXTRA_BUILD  ?= ${BIN_AYACC}

SPEC=${PREFIX}/include/gela-ada-spec
GPR=${PREFIX}/lib/gnat/gela_asis.gpr

# Not used for now
# Input buffer method:
ADA_INPUT_BUFFER ?= Portable

all: ${LIB_ASIS}

generate: make_dirs ${ASIS_SRC} ${LEXER_SRC} ${PARSER_SRC} ${BUFFER_SRC}

make_dirs: ${WORK_DIRS}

redistr:
	tar cf - -C ${SVN_PATH} $(EXCLUDE) ${SVN_DIRS} | tar xf -
	mkdir doc
	curl -o doc/gela_asis_ug.txt ${UG_URL}


${WORK_DIRS}:
	mkdir -p $@

${BUFFER_SRC}:
	echo with Gela.Source_Buffers.${ADA_INPUT_BUFFER}\; > ${BUFFER_SRC}
	echo package Gela.Source_Buffers.Current renames \
                Gela.Source_Buffers.${ADA_INPUT_BUFFER}\; \
	>> ${BUFFER_SRC}

${XML_TO_Y}: ${XML_TO_Y_SRC} ${LIBGELA_SRC} ${ENCODING_SRC} ${XML_IO_SRC}
	${BIN_GNATMAKE} ${FLAGS} -D ${BUILD}/xml2ayacc \
	 -Isrc/utilities/xml2ayacc \
	 -I${A}/libgela -I${A}/encoding -I${A}/encoding/auto \
	 -I${A}/xml_io \
	 -o ${XML_TO_Y} xml_to_y.adb

${UAFLEX_BIN}: ${UAFLEX_SRC}
	${BIN_GNATMAKE} ${FLAGS} -D ${BUILD}/uaflex -Isrc/utilities/uaflex \
	   -o ${UAFLEX_BIN} uaflex.adb

${LEXER_SRC}: ${UAFLEX_BIN} ${UAFLEX}/ada.uaflex
	${UAFLEX_BIN} ${UAFLEX}/ada.uaflex Asis.Gela.Scanner_Tables > \
	   ${L}/lexer.all
	$(BIN_GNATCHOP) -w ${L}/lexer.all ${BUILD}/ada


${PARSER_SRC}: ${XML_TO_Y} \
               ${XML}/fixed.xsl      ${XML}/syntax.xml                   \
               ${XML}/asis_hier.xml  ${XML}/tokens.xml   ${XML}/code.xml \
               ${AYACC}/deep.sed     ${AYACC}/asis-gela-parser.adt       \
               ${EXTRA_BUILD}                                            \

	echo %token New_Line_Token                     > ${P}/parser.y
	echo %token Separator_Token                   >> ${P}/parser.y
	echo %token Comment_Token                     >> ${P}/parser.y
	$(BIN_XSLTPROC) -o ${P}/fixed.xml ${XML}/fixed.xsl ${XML}/syntax.xml
	${XML_TO_Y} ${XML}/asis_hier.xml ${XML}/tokens.xml ${P}/fixed.xml \
           ${XML}/code.xml >> ${P}/parser.y
	$(BIN_CAT) ${AYACC}/asis-gela-parser.adt >> ${P}/parser.y
	$(BIN_AYACC) ${P}/parser.y
	$(BIN_SED) -f ${AYACC}/deep.sed ${P}/parser*.ad[sb] > ${P}/parser.all
	$(BIN_GNATCHOP) -w ${P}/parser.all ${BUILD}/ada


${ASIS_SRC}: ${XML}/asis_hier.xml ${ASIS}/asis-gela-elements.xsl \
             ${ASIS}/asis.spec    ${ASIS}/asis-ads.xsl           \
             ${ASIS}/asis.body    ${ASIS}/asis-adb.xsl           \

	$(BIN_CP)       ${ASIS}/asis.spec ${BUILD}/ada/asis.ads
	$(BIN_XSLTPROC) ${ASIS}/asis-ads.xsl ${XML}/asis_hier.xml >> \
	   ${BUILD}/ada/asis.ads
	$(BIN_CP)       ${ASIS}/asis.body ${BUILD}/ada/asis.adb
	$(BIN_XSLTPROC) ${ASIS}/asis-adb.xsl ${XML}/asis_hier.xml >> \
	   ${BUILD}/ada/asis.adb
	$(BIN_XSLTPROC) -o ${BUILD}/asis/elems.all \
	   ${ASIS}/asis-gela-elements.xsl \
           ${XML}/asis_hier.xml
	$(BIN_GNATCHOP) -w ${BUILD}/asis/elems.all ${BUILD}/ada

${LIB_ASIS}: ${WORK_DIRS}
	${BIN_GNATMAKE} ${FLAGS} -c -u -O0 -D ${OBJ_DIR} \
	 -I${BUILD}/ada -I${A}/libgela -I${A}/xasis -I${A}/asis \
	 asis-gela-parser.adb
	${BIN_GNATMAKE} ${FLAGS} -m -D ${OBJ_DIR} \
	 -I${BUILD}/ada -I${A}/libgela -I${A}/xasis \
	 ${A}/asis/asis-[^g]*.adb
	ar cr ${LIB_ASIS} ${OBJ_DIR}/*.o
	ranlib ${LIB_ASIS}

install: ${LIB_ASIS}
	${BIN_INSTALL} -d ${PREFIX}/include/gela-asis
	${BIN_INSTALL} -d ${SPEC}
	${BIN_INSTALL} -d ${PREFIX}/lib/gela-asis
	${BIN_INSTALL} -d ${PREFIX}/lib/gnat
	${BIN_INSTALL} -m 644 ${BUILD}/ada/*.ad[sb] ${PREFIX}/include/gela-asis
	${BIN_INSTALL} -m 644 ${A}/libgela/*.ad[sb] ${PREFIX}/include/gela-asis
	${BIN_INSTALL} -m 644 ${A}/xasis/*.ad[sb]   ${PREFIX}/include/gela-asis
	${BIN_INSTALL} -m 644 ${A}/asis/*.ad[sb]    ${PREFIX}/include/gela-asis
	${BIN_INSTALL} -m 444 ${OBJ_DIR}/*.ali      ${PREFIX}/lib/gela-asis
	${BIN_INSTALL} -m 644 ${LIB_ASIS}           ${PREFIX}/lib/gela-asis
	${BIN_INSTALL} -m 644 src/lib/libada/spec/*.ads              ${SPEC}
	${BIN_INSTALL} -m 644 src/lib/libada/spec/annex*/*.ads       ${SPEC}
	${BIN_INSTALL} -m 644 src/lib/libada/spec/obsolescent*/*.ads ${SPEC}
	@${BIN_ECHO} "project Gela_ASIS is"                    > ${GPR}
	@${BIN_ECHO} "   for Languages use (\"Ada\");"         >> ${GPR}
	@${BIN_ECHO} \
          "   for Source_Dirs use (\"${PREFIX}/include/gela-asis\");" >> ${GPR}
	@${BIN_ECHO} "   for Library_Name use \"asis\";"       >> ${GPR}
	@${BIN_ECHO} \
          "   for Library_Dir use \"${PREFIX}/lib/gela-asis\";" >> ${GPR}
	@${BIN_ECHO} "   for Externally_Built use \"True\";"   >> ${GPR}
	@${BIN_ECHO} "end Gela_ASIS;"                          >> ${GPR}

	@${BIN_ECHO} Please add next variable to your environmet:
	@${BIN_ECHO} GELA_INCLUDE_PATH=${SPEC}

aflex: make_dirs
	${BIN_GNATMAKE} ${FLAGS} -D ${BUILD}/aflex \
	 -Iaflex/src \
	 -o ${L}/aflex aflex.adb

${BIN_AYACC}: contrib/ayacc/src/*.ad[sb]
	${BIN_GNATMAKE} ${FLAGS} -D ${BUILD}/ayacc \
	 -Icontrib/ayacc/src \
	 -o ${BIN_AYACC} ayacc.adb

clean:
	rm -rf ${WORK_DIRS}

AV_SRC  := examples/asis_view_gtk
AV_OUT  := ${BUILD}/asis_view
AV_HIER := ${AV_OUT}/hier.xml

asis_view: ${AV_OUT}
	$(BIN_XSLTPROC) -o ${AV_HIER} ${AV_SRC}/rm_abs.xsl ${XML}/asis_hier.xml
	$(BIN_XSLTPROC) -o ${AV_OUT}/gtk_asis_element_frames.ads \
	    ${AV_SRC}/frames.xsl ${AV_HIER}
	$(BIN_XSLTPROC) -o ${AV_OUT}/gtk_asis_elements-create_frames.adb \
	    ${AV_SRC}/create.xsl ${AV_HIER}
	$(BIN_XSLTPROC) -o ${AV_OUT}/gtk_asis_elements-show.adb \
	    ${AV_SRC}/show.xsl ${AV_HIER}
	${BIN_GNATMAKE} -P ${AV_SRC}/asis_view.gpr

${AV_OUT}:
	mkdir -p ${AV_OUT}
