------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--        Library for dealing with grammars for for Gela project,           --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Lexical.Handlers;
with Gela.Lexical.Scanners;
with Gela.Lexical.Tokens;
with Gela.Lexical.Types;

package Gela.Lexical.Handler is

   procedure Initialize;

   type Handler is new Gela.Lexical.Handlers.Handler with null record;

   procedure Identifier
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   procedure Numeric_Literal
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   procedure Character_Literal
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   procedure String_Literal
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   procedure Comment
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   procedure Space
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   procedure New_Line
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   procedure Error
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   procedure Delimiter
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

end Gela.Lexical.Handler;
