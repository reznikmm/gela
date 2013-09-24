------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Lexical.Tokens;
with Gela.Types;

package Gela.Tokens is
   pragma Preelaborate;

   type Token is interface;

   function Assigned
     (Self    : access Token'Class;
      Payload : Gela.Types.Payload)
      return Boolean is (Self /= null);

   not overriding function Value
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Tokens.Token is abstract;

   not overriding function Line
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Line_Index is abstract;

   not overriding function First
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Text_Index is abstract;

   not overriding function Last
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Text_Index is abstract;

   not overriding function Separator
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Text_Index is abstract;

   not overriding function Compilation
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Types.Compilation_Access is abstract;

   not overriding function Symbol
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Types.Symbol is abstract;

end Gela.Tokens;
