--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------
--
-- In Allow_Char  state character literals are available
-- Lexer expected
--   to enter in Default after identifier
--      [so no character literal are allowed, example: Character'('Y')]
--   to enter in Allow_Char after others tokens

%s Allow_Char

new_line           \r\n|[\n\v\f\u0085[:Zl:][:Zp:]]

identifier_start   [[:Lu:][:Ll:][:Lt:][:Lm:][:Lo:][:Nl:]]
--  identifier_extend without punctuation_connector
identifier_extend  [[:Mn:][:Mc:][:Nd:]]
identifier         {identifier_start}([:Pc:]?({identifier_start}|{identifier_extend}))*

digit              [0-9]
numeral            ({digit}(_?{digit})*)
exponent           ([Ee]\+?{numeral}|[Ee]-{numeral})
decimal_literal    ({numeral}(\.{numeral})?{exponent}?)
extended_digit     [0-9a-fA-F]
base               {numeral}
based_numeral      ({extended_digit}(_?{extended_digit})*)
based_literal      ({base}#{based_numeral}(\.{based_numeral})?#{exponent}?)
numeric_literal    ({decimal_literal}|{based_literal})
based_literal_obs  ({base}:{based_numeral}(\.{based_numeral})?:{exponent}?)

non_quotation_mark_graphic_character [^"%[:Cc:][:Co:][:Cs:]\t\n\v\f\u0085[:Zl:][:Zp:]\uFFFE\uFFFF\U0001FFFE\U0001FFFF\U0002FFFE\U0002FFFF\U0003FFFE\U0003FFFF\U0004FFFE\U0004FFFF\U0005FFFE\U0005FFFF\U0006FFFE\U0006FFFF\U0007FFFE\U0007FFFF\U0008FFFE\U0008FFFF\U0009FFFE\U0009FFFF\U000AFFFE\U000AFFFF\U000BFFFE\U000BFFFF\U000CFFFE\U000CFFFF\U000DFFFE\U000DFFFF\U000EFFFE\U000EFFFF\U000FFFFE\U000FFFFF\U0010FFFE\U0010FFFF]
graphic_character  ({non_quotation_mark_graphic_character}|\"|%)
character_literal  '{graphic_character}'

string_element     (\"\"|{non_quotation_mark_graphic_character}|%)
string_literal     \"{string_element}*\"
string_element_obs (%%|{non_quotation_mark_graphic_character})
string_literal_obs %{string_element}*%

comment            \-\-[^\n\v\f\u0085[:Zl:][:Zp:]]*

separator          [[:Zs:][:Cf:]\t]+
%%

"=>"	{Delimiter}
".."	{Delimiter}
"**"	{Delimiter}
":="	{Delimiter}
"/="	{Delimiter}
">="	{Delimiter}
"<="	{Delimiter}
"<<"	{Delimiter}
">>"	{Delimiter}
"<>"	{Delimiter}
"&"     {Delimiter}
"'"     {Delimiter}
"("     {Delimiter}
")"     {Delimiter}
"*"     {Delimiter}
"+"     {Delimiter}
","     {Delimiter}
"-"     {Delimiter}
"."     {Delimiter}
"/"     {Delimiter}
":"     {Delimiter}
";"     {Delimiter}
"<"     {Delimiter}
"="     {Delimiter}
">"     {Delimiter}
"|"     {Delimiter}
"!"     {Delimiter}

{identifier}                     {Identifier}
{numeric_literal}                {Numeric_Literal}
{based_literal_obs}              {Obsolescent_Numeric_Literal}
<Allow_Char>{character_literal}  {Character_Literal}
{string_literal}                 {String_Literal}
{string_literal_obs}             {Obsolescent_String_Literal}
{comment}                        {Comment}
{separator}                      {Space}
{new_line}                       {New_Line}
--  ARM2.2p7 At least one separator is required ...
{identifier}{numeric_literal}  {Error}
{numeric_literal}{identifier}  {Error}


%%
