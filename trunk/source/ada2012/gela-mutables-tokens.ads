------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Tokens;
with Gela.Types;
with Gela.Lexical.Tokens;
with Gela.Mutables.Elements;

package Gela.Mutables.Tokens is

   type Token is
     new Gela.Mutables.Elements.Element and Gela.Tokens.Token
     with null record;

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

   overriding function Child
     (Self    : access Token;
      Payload : Gela.Types.Payload;
      Index   : Positive) return Gela.Types.Payload is (0);

   overriding procedure Set_Child
     (Self    : access Token;
      Payload : Gela.Types.Payload;
      Index   : Positive;
      Value   : Gela.Types.Payload) is null;

   not overriding procedure Create
     (Self      : access Token;
      Result    : out Gela.Types.Payload;
      Value     : Gela.Lexical.Tokens.Token;
      Line      : Gela.Lexical.Line_Index;
      First     : Gela.Lexical.Text_Index;
      Last      : Gela.Lexical.Text_Index;
      Separator : Gela.Lexical.Text_Index;
      Symbol    : Gela.Types.Symbol);

end Gela.Mutables.Tokens;
