------------------------------------------------------------------------------
--                        G E L A   G R A M M A R S                         --
--        Library for dealing with grammars for for Gela project,           --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Strings;

with Gela.Lexical.Tokens;

package Gela.Lexical.Fabrics is

   type Fabric is limited interface;

   type Fabric_Access is access all Fabric'Class;

   procedure New_Line
     (Self    : access Fabric;
      First   : Text_Index;  --  Position of first character of line
      Last    : Text_Index;  --  Position of last character of line
      Comment : Text_Index)  --  Position of first character of comment in line
   is abstract;

   procedure New_Token
     (Self      : access Fabric;
      Token     : Gela.Lexical.Tokens.Token;
      Line      : Positive;    --  Line of token
      First     : Text_Index;  --  Position of first character of token
      Last      : Text_Index;  --  Position of last character of token
      Separator : Text_Index;  --  Position of first character of separator
      Folded    : League.Strings.Universal_String)  --  Folded identifier img
   is abstract;

end Gela.Lexical.Fabrics;
