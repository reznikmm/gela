------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;
with String_Sources;

with Gela.Grammars.Lexers;
with Gela.Lexical.Fabrics;
with Gela.Lexical.Tokens;
with Gela.Lexical.Handler;
with Gela.Lexical.Scanners;
with Gela.Types;
with Gela.Nodes;

package Gela.Mutables.Lexers is
   pragma Preelaborate;

   type Lexer (Compilation : Mutable_Compilation_Access)
     is limited new Gela.Lexical.Fabrics.Fabric
       and Gela.Grammars.Lexers.Lexer with private;

   not overriding procedure Initialize
     (Self : access Lexer;
      Text : League.Strings.Universal_String);

   not overriding function Get_Token
     (Self  : access Lexer;
      Index : Positive)
      return Gela.Types.Token;

   not overriding procedure Next_Token
     (Self  : access Lexer;
      Token : out Gela.Grammars.Terminal_Count;
      Value : out Gela.Nodes.Element);

   not overriding function Last_Line
     (Self : access Lexer) return Gela.Lexical.Line_Count;

   not overriding function Line
     (Self  : access Lexer;
      Index : Gela.Lexical.Line_Index) return Gela.Lexical.Line_Offset;

   not overriding function Last_Token
     (Self : access Lexer) return Gela.Types.Payload;

private

   type Line_Offset_Array is array
     (Gela.Lexical.Line_Index range <>) of Gela.Lexical.Line_Offset;
   type Line_Offset_Array_Access is access all Line_Offset_Array;

   type Payload_Array_Access is access all Gela.Types.Payload_Array;

   type Lexer  (Compilation : Mutable_Compilation_Access)
     is limited new Gela.Lexical.Fabrics.Fabric
       and Gela.Grammars.Lexers.Lexer with
   record
         --  Properties
         Lines       : Line_Offset_Array_Access;
         Last_Line   : Gela.Lexical.Line_Count;
         Tokens      : Payload_Array_Access;
         Last_Token  : Natural := 0;
         --  Components
         Scanner     : aliased Gela.Lexical.Scanners.Scanner;
         Source      : aliased String_Sources.String_Source;
         Handler     : aliased Gela.Lexical.Handler.Handler (Compilation);
   end record;

   overriding function Next
     (Self : in out Lexer) return Gela.Grammars.Terminal_Count;

   subtype Text_Index is Gela.Lexical.Text_Index;

   overriding procedure New_Line
     (Self    : access Lexer;
      First   : Text_Index;  --  Position of first character of line
      Last    : Text_Index;  --  Position of last character of line
      Comment : Text_Index); --  Position of first character of comment in line

   overriding procedure New_Token
     (Self      : access Lexer;
      Token     : Gela.Lexical.Tokens.Token;
      Line      : Positive;    --  Line of token
      First     : Text_Index;  --  Position of first character of token
      Last      : Text_Index;  --  Position of last character of token
      Separator : Text_Index;  --  Position of first character of separator
      Folded    : League.Strings.Universal_String); --  Folded identifier img

end Gela.Mutables.Lexers;
