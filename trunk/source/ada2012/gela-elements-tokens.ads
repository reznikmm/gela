------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Lexical.Tokens;

package Gela.Elements.Tokens is

   type Token is abstract new Fly_Weight_Object with null record;

   not overriding function Value
     (Self    : Token;
      Payload : Gela.Elements.Payload)
      return Gela.Lexical.Tokens.Token is abstract;

   not overriding function Line
     (Self    : Token;
      Payload : Gela.Elements.Payload)
      return Gela.Lexical.Line_Index is abstract;

   not overriding function First
     (Self    : Token;
      Payload : Gela.Elements.Payload)
      return Gela.Lexical.Text_Index is abstract;

   not overriding function Last
     (Self    : Token;
      Payload : Gela.Elements.Payload)
      return Gela.Lexical.Text_Index is abstract;

   not overriding function Separator
     (Self    : Token;
      Payload : Gela.Elements.Payload)
      return Gela.Lexical.Text_Index is abstract;

end Gela.Elements.Tokens;
