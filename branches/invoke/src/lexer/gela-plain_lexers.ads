with League.Strings;

with Gela.Lexers;
with Gela.Symbol_Sets;

package Gela.Plain_Lexers is
   pragma Preelaborate;

   type Lexer is new Gela.Lexers.Lexer with null record;

   overriding procedure Scan
     (Self    : Lexer;
      Input   : League.Strings.Universal_String;
      Symbols : not null access Gela.Symbol_Sets.Symbol_Set'Class;
      Output  : not null access Gela.Lexers.Lexer_Destination'Class);

end Gela.Plain_Lexers;
