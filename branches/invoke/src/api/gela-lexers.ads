--  This package provides Lexer/Lexer_Destination interfaces and methods.

with League.Strings;

with Gela.Lexical_Types;

package Gela.Lexers is
   pragma Preelaborate;

   type Lexer_Destination is limited interface;
   --  Type to report result of lexic analysis

   not overriding procedure New_Token
     (Self  : in out Lexer_Destination;
      Token : Gela.Lexical_Types.Token) is abstract;
   --  Called when new token found during lexic analysis

   not overriding procedure New_Line
     (Self  : in out Lexer_Destination;
      Line  : Gela.Lexical_Types.Line_Span) is abstract;
   --  Called when new end of line found during lexic analysis

   type Lexer is limited interface;
   type Lexer_Access is access all Lexer'Class;
   for Lexer_Access'Storage_Size use 0;

   not overriding procedure Scan
     (Self    : Lexer;
      Input   : League.Strings.Universal_String;
      Output  : not null access Lexer_Destination'Class) is abstract;
   --  Analyse gigen Input and report events to Output

end Gela.Lexers;
