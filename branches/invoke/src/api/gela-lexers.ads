with League.Strings;

with Gela.Lexical_Types;
with Gela.Symbol_Sets;

package Gela.Lexers is
   pragma Preelaborate;

   type Lexer_Destination is limited interface;

   not overriding procedure New_Token
     (Self  : in out Lexer_Destination;
      Token : Gela.Lexical_Types.Token) is abstract;

   not overriding procedure New_Line
     (Self  : in out Lexer_Destination;
      Line  : Gela.Lexical_Types.Line_Span) is abstract;

   type Lexer is limited interface;
   type Lexer_Access is access all Lexer'Class;
   for Lexer_Access'Storage_Size use 0;

   not overriding procedure Scan
     (Self    : Lexer;
      Input   : League.Strings.Universal_String;
      Symbols : not null access Gela.Symbol_Sets.Symbol_Set'Class;
      Output  : not null access Lexer_Destination'Class) is abstract;

end Gela.Lexers;
