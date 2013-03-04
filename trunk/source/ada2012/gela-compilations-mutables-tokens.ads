------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Tokens;

package Gela.Compilations.Mutables.Tokens is

   type Token is new Gela.Tokens.Token with record
      Compilation : Mutable_Compilation_Access;
   end record;

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

   overriding function Next_Token
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Types.Token;

   not overriding procedure Create
     (Self      : access Token;
      Result    : out Gela.Types.Payload;
      Value     : Gela.Lexical.Tokens.Token;
      Line      : Gela.Lexical.Line_Index;
      First     : Gela.Lexical.Text_Index;
      Last      : Gela.Lexical.Text_Index;
      Separator : Gela.Lexical.Text_Index;
      Symbol    : Gela.Types.Symbol);
   --  FIXME Pass prev token to initialize list of tokens

end Gela.Compilations.Mutables.Tokens;
