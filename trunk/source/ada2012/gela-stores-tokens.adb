------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Mutables.Compilations;

package body Gela.Stores.Tokens is

   Ref : constant Boolean := Gela.Mutables.Compilations.Dummy_Reference;
   pragma Unreferenced (Ref);

   package Offset is
      Tag       : constant := 0;
      Count     : constant := Tag + 1;
      Value     : constant := Count + 1;
      Line      : constant := Value + 1;
      First     : constant := Line + 1;
      Last      : constant := First + 1;
      Separator : constant := Last + 1;
      --  Symbol    : constant := Separator + 1;
   end Offset;

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
        Self.Compilation.Store.Get (Item + Offset.First);
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
      Separator : Gela.Lexical.Text_Index)
   is
      Item  : constant Index := Index (Payload);
   begin
      Self.Compilation.Store.Set
        (Item + Offset.Value, Gela.Lexical.Tokens.Token'Pos (Value));
      Self.Compilation.Store.Set (Item + Offset.Line, Element (Line));
      Self.Compilation.Store.Set (Item + Offset.First, Element (First));
      Self.Compilation.Store.Set (Item + Offset.Last, Element (Last));
      Self.Compilation.Store.Set
        (Item + Offset.Separator, Element (Separator));
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
        Self.Compilation.Store.Get (Item + Offset.Last);
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
        Self.Compilation.Store.Get (Item + Offset.Line);
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
        Self.Compilation.Store.Get (Item + Offset.Separator);
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
      return Offset.Separator + 1;
   end Size;

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
        Self.Compilation.Store.Get (Item + Offset.Value);
   begin
      return Gela.Lexical.Tokens.Token'Val (Value);
   end Value;

end Gela.Stores.Tokens;
