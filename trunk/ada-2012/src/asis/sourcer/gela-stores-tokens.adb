------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Compilations;
with Gela.Mutables.Compilations;
pragma Unreferenced (Gela.Mutables.Compilations);

package body Gela.Stores.Tokens is

   package Offset is
      Tag       : constant := 0;
      Count     : constant := Tag + 1;
      Value     : constant := Count + 1;
      Line      : constant := Value + 1;
      First     : constant := Line + 1;
      Last      : constant := First + 1;
      Separator : constant := Last + 1;
      Symbol    : constant := Separator + 1;
   end Offset;

   -----------------
   -- Compilation --
   -----------------

   overriding function Compilation
     (Self    : access Token;
      Payload : Gela.Types.Payload) return Gela.Types.Compilation_Access
   is
      pragma Unreferenced (Payload);
      Result : constant Gela.Compilations.Abstract_Compilation_Access :=
        Gela.Compilations.Abstract_Compilation_Access (Self.Store.Compilation);
   begin
      return Gela.Types.Compilation_Access (Result);
   end Compilation;

   -----------
   -- First --
   -----------

   overriding function First
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Text_Index
   is
      Item  : constant Index := Index (Payload);
      Value : constant Element :=
        Self.Get (Item + Offset.First);
   begin
      return Gela.Lexical.Text_Index (Value);
   end First;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self      : access Token;
      Payload   : Gela.Types.Payload;
      Value     : Gela.Lexical.Tokens.Token;
      Line      : Gela.Lexical.Line_Index;
      First     : Gela.Lexical.Text_Index;
      Last      : Gela.Lexical.Text_Index;
      Separator : Gela.Lexical.Text_Index;
      Symbol    : Gela.Types.Symbol)
   is
      Item  : constant Index := Index (Payload);
   begin
      Self.Set (Item + Offset.Value, Gela.Lexical.Tokens.Token'Pos (Value));
      Self.Set (Item + Offset.Line, Element (Line));
      Self.Set (Item + Offset.First, Element (First));
      Self.Set (Item + Offset.Last, Element (Last));
      Self.Set (Item + Offset.Separator, Element (Separator));
      Self.Set (Item + Offset.Symbol, Element (Symbol));
   end Initialize;

   ----------
   -- Last --
   ----------

   overriding function Last
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Text_Index
   is
      Item  : constant Index := Index (Payload);
      Value : constant Element :=
        Self.Get (Item + Offset.Last);
   begin
      return Gela.Lexical.Text_Index (Value);
   end Last;

   ----------
   -- Line --
   ----------

   overriding function Line
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Line_Index
   is
      Item  : constant Index := Index (Payload);
      Value : constant Element :=
        Self.Get (Item + Offset.Line);
   begin
      return Gela.Lexical.Line_Index (Value);
   end Line;

   ---------------
   -- Separator --
   ---------------

   overriding function Separator
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Text_Index
   is
      Item  : constant Index := Index (Payload);
      Value : constant Element :=
        Self.Get (Item + Offset.Separator);
   begin
      return Gela.Lexical.Text_Index (Value);
   end Separator;

   ----------
   -- Size --
   ----------

   overriding function Size
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Natural
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Payload);
   begin
      return Offset.Symbol + 1;
   end Size;

   ------------
   -- Symbol --
   ------------

   overriding function Symbol
     (Self    : access Token;
      Payload : Gela.Types.Payload) return Gela.Types.Symbol
   is
      Item  : constant Index := Index (Payload);
      Value : constant Element := Self.Get (Item + Offset.Symbol);
   begin
      return Gela.Types.Symbol (Value);
   end Symbol;

   -----------
   -- Value --
   -----------

   overriding function Value
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Tokens.Token
   is
      Item  : constant Index := Index (Payload);
      Value : constant Element :=
        Self.Get (Item + Offset.Value);
   begin
      return Gela.Lexical.Tokens.Token'Val (Value);
   end Value;

end Gela.Stores.Tokens;
