              Gela ASIS: Portable ASIS Implementation
              ---------------------------------------
                   Version 0.3.0   05 Nov 2010
                http://www.ten15.org/wiki/gela_asis

Gela ASIS is platform/compiler independent implementation of Ada
Semantic Interface Specification (ASIS). Gela ASIS implement
core ASIS Version 2.0 and most of ASIS Issues (SI99), so it's
capable to process Ada 2005 code.

Read installation and usage instruction in doc/gela_asis_ug.txt

Changes since Gela 0.2.0:
 * Migrate from aflex to myown Unicode aware scanner
 * Support UTF-8 and meny others input source encodings
 * Asis.Text implemented
 * Bug fixes

ASIS_View
---------

Simple ASIS viewer is included in examples. It requires GtkAda 2.10
and Gela ASIS installed. To build it run
> make asis_view

Resultin program is .build/asis_view/asis_view_gtk-run

