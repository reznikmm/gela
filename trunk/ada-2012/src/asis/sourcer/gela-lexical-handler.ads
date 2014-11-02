------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--        Library for dealing with grammars for for Gela project,           --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;

with Gela.Lexical.Handlers;
with Gela.Lexical.Scanners;
with Gela.Lexical.Tokens;
with Gela.Lexical.Types;
with Gela.Lexical.Fabrics;
with Gela.Mutables;

package Gela.Lexical.Handler is
   pragma Preelaborate;

   procedure Initialize;

   type Handler (Compilation : Gela.Mutables.Mutable_Compilation_Access)
     is new Gela.Lexical.Handlers.Handler with private;

   procedure Set_Fabric
     (Self   : in out Handler;
      Fabric : not null Gela.Lexical.Fabrics.Fabric_Access);

   overriding procedure Identifier
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   overriding procedure Numeric_Literal
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   overriding procedure Obsolescent_Numeric_Literal
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   overriding procedure Character_Literal
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   overriding procedure String_Literal
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   overriding procedure Obsolescent_String_Literal
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   overriding procedure Comment
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   overriding procedure Space
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   overriding procedure New_Line
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   overriding procedure Error
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   overriding procedure Delimiter
     (Self    : not null access Handler;
      Scanner : not null access Gela.Lexical.Scanners.Scanner'Class;
      Rule    : Gela.Lexical.Types.Rule_Index;
      Token   : out Gela.Lexical.Tokens.Token;
      Skip    : in out Boolean);

   function Unfolded_To_Token
     (X : League.Strings.Universal_String)
      return Gela.Lexical.Tokens.Token;
   --  Convert text to keywoard. Text could be unfolded before call this.

private

   subtype Text_Index is Gela.Lexical.Text_Index;

   type Handler (Compilation : Gela.Mutables.Mutable_Compilation_Access)
     is new Gela.Lexical.Handlers.Handler with record
      Last_Token : Gela.Lexical.Tokens.Token := Tokens.Token'First;
      Last       : Text_Index := 1;  --  Position of last character
      Line       : Positive := 1;    --  Last line number
      Line_First : Text_Index := 1;  --  Position of first character of line
      Comment    : Text_Index := 0;  --  Position of first character of comment
      Separator  : Text_Index := 1;  --  Position of first character of separ
      Fabric     : Gela.Lexical.Fabrics.Fabric_Access;
   end record;

end Gela.Lexical.Handler;
