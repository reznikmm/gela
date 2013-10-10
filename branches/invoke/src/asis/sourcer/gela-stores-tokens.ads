------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Nodes.Tokens;
with Gela.Types;
with Gela.Lexical.Tokens;
with Gela.Stores.Nodes;

package Gela.Stores.Tokens is
   pragma Preelaborate;

   type Token is
     new Gela.Stores.Nodes.Node and Gela.Nodes.Tokens.Object
     with null record;

   type Token_Access is access all Token'Class;

   overriding function Value
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Tokens.Token;

   overriding function Line
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Line_Index;

   overriding function First
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Text_Index;

   overriding function Last
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Text_Index;

   overriding function Separator
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Text_Index;

   overriding function Last_Child
     (Self    : access Token;
      Payload : Gela.Types.Payload) return Natural is (0);

   overriding function Size
     (Self    : access Token;
      Payload : Gela.Types.Payload) return Natural;

   overriding function Compilation
     (Self    : access Token;
      Payload : Gela.Types.Payload) return Gela.Types.Compilation_Access;

   overriding function Symbol
     (Self    : access Token;
      Payload : Gela.Types.Payload) return Gela.Types.Symbol;

   not overriding procedure Initialize
     (Self      : access Token;
      Payload   : Gela.Types.Payload;
      Value     : Gela.Lexical.Tokens.Token;
      Line      : Gela.Lexical.Line_Index;
      First     : Gela.Lexical.Text_Index;
      Last      : Gela.Lexical.Text_Index;
      Separator : Gela.Lexical.Text_Index;
      Symbol    : Gela.Types.Symbol);

end Gela.Stores.Tokens;
