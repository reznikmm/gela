------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--        Library for dealing with grammars for for Gela project,           --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Lexers;
with Gela.Scanner_Handlers;
with Gela.Scanners;
with Gela.Lexical_Types;
with Gela.Scanner_Types;
with Gela.Symbol_Sets;
package Gela.Lexical_Handler is
   pragma Preelaborate;

   procedure Initialize;

   type Handler
     (Output  : not null access Gela.Lexers.Lexer_Destination'Class;
      Symbols : not null access Gela.Symbol_Sets.Symbol_Set'Class)
     is new Gela.Scanner_Handlers.Handler with private;

   overriding procedure Identifier
     (Self    : not null access Handler;
      Scanner : not null access Gela.Scanners.Scanner'Class;
      Rule    : Gela.Scanner_Types.Rule_Index;
      Token   : out Gela.Lexical_Types.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Numeric_Literal
     (Self    : not null access Handler;
      Scanner : not null access Gela.Scanners.Scanner'Class;
      Rule    : Gela.Scanner_Types.Rule_Index;
      Token   : out Gela.Lexical_Types.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Obsolescent_Numeric_Literal
     (Self    : not null access Handler;
      Scanner : not null access Gela.Scanners.Scanner'Class;
      Rule    : Gela.Scanner_Types.Rule_Index;
      Token   : out Gela.Lexical_Types.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Character_Literal
     (Self    : not null access Handler;
      Scanner : not null access Gela.Scanners.Scanner'Class;
      Rule    : Gela.Scanner_Types.Rule_Index;
      Token   : out Gela.Lexical_Types.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure String_Literal
     (Self    : not null access Handler;
      Scanner : not null access Gela.Scanners.Scanner'Class;
      Rule    : Gela.Scanner_Types.Rule_Index;
      Token   : out Gela.Lexical_Types.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Obsolescent_String_Literal
     (Self    : not null access Handler;
      Scanner : not null access Gela.Scanners.Scanner'Class;
      Rule    : Gela.Scanner_Types.Rule_Index;
      Token   : out Gela.Lexical_Types.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Comment
     (Self    : not null access Handler;
      Scanner : not null access Gela.Scanners.Scanner'Class;
      Rule    : Gela.Scanner_Types.Rule_Index;
      Token   : out Gela.Lexical_Types.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Space
     (Self    : not null access Handler;
      Scanner : not null access Gela.Scanners.Scanner'Class;
      Rule    : Gela.Scanner_Types.Rule_Index;
      Token   : out Gela.Lexical_Types.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure New_Line
     (Self    : not null access Handler;
      Scanner : not null access Gela.Scanners.Scanner'Class;
      Rule    : Gela.Scanner_Types.Rule_Index;
      Token   : out Gela.Lexical_Types.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Error
     (Self    : not null access Handler;
      Scanner : not null access Gela.Scanners.Scanner'Class;
      Rule    : Gela.Scanner_Types.Rule_Index;
      Token   : out Gela.Lexical_Types.Token_Kind;
      Skip    : in out Boolean);

   overriding procedure Delimiter
     (Self    : not null access Handler;
      Scanner : not null access Gela.Scanners.Scanner'Class;
      Rule    : Gela.Scanner_Types.Rule_Index;
      Token   : out Gela.Lexical_Types.Token_Kind;
      Skip    : in out Boolean);

private

   subtype Text_Index is Gela.Lexical_Types.Text_Index;

   type Handler (Output  : not null access Gela.Lexers.Lexer_Destination'Class;
                 Symbols : not null access Gela.Symbol_Sets.Symbol_Set'Class)
     is new Gela.Scanner_Handlers.Handler with record
      Last_Token : Gela.Lexical_Types.Token_Kind :=
        Gela.Lexical_Types.Token_Kind'First;
      Last       : Text_Index := 1;  --  Position of last character
      Line       : Gela.Lexical_Types.Line_Count := 1;  --  Last line number
      Line_First : Text_Index := 1;  --  Position of first character of line
      Comment    : Text_Index := 0;  --  Position of first character of comment
      Separator  : Text_Index := 1;  --  Position of first character of separ
   end record;

end Gela.Lexical_Handler;
