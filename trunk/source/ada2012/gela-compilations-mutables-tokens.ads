------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Elements.Tokens;

package Gela.Compilations.Mutables.Tokens is

   type Token is new Gela.Elements.Tokens.Token with record
      Compilation : Mutable_Compilation_Access;
   end record;

   overriding function Value
     (Self    : Token;
      Payload : Gela.Elements.Payload)
      return Gela.Lexical.Tokens.Token;

   overriding function Line
     (Self    : Token;
      Payload : Gela.Elements.Payload)
      return Gela.Lexical.Line_Index;

   overriding function First
     (Self    : Token;
      Payload : Gela.Elements.Payload)
      return Gela.Lexical.Text_Index;

   overriding function Last
     (Self    : Token;
      Payload : Gela.Elements.Payload)
      return Gela.Lexical.Text_Index;

   overriding function Separator
     (Self    : Token;
      Payload : Gela.Elements.Payload)
      return Gela.Lexical.Text_Index;

   not overriding procedure Create
     (Self      : Token;
      Result    : out Gela.Elements.Payload;
      Value     : Gela.Lexical.Tokens.Token;
      Line      : Gela.Lexical.Line_Index;
      First     : Gela.Lexical.Text_Index;
      Last      : Gela.Lexical.Text_Index;
      Separator : Gela.Lexical.Text_Index);

end Gela.Compilations.Mutables.Tokens;
