with League.Strings;

with Gela.Lexers;
with Gela.Contexts;

package Gela.Plain_Lexers is
   pragma Preelaborate;

   type Lexer (Context : access Gela.Contexts.Context'Class) is
     limited new Gela.Lexers.Lexer with null record;

   overriding procedure Scan
     (Self    : Lexer;
      Input   : League.Strings.Universal_String;
      Output  : not null access Gela.Lexers.Lexer_Destination'Class);

end Gela.Plain_Lexers;
