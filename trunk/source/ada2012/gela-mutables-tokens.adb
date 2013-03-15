------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Properties.Tokens;
with Gela.Relocatable_Arrays;
with Gela.Mutables.Compilations;

package body Gela.Mutables.Tokens is

   package Offset renames Gela.Properties.Tokens;

   Token_Size : constant Gela.Relocatable_Arrays.Index := Offset.Size;

   ------------
   -- Create --
   ------------

   not overriding procedure Create
     (Self      : access Token;
      Result    : out Gela.Types.Payload;
      Value     : Gela.Lexical.Tokens.Token;
      Line      : Gela.Lexical.Line_Index;
      First     : Gela.Lexical.Text_Index;
      Last      : Gela.Lexical.Text_Index;
      Separator : Gela.Lexical.Text_Index;
      Symbol    : Gela.Types.Symbol)
   is
      use type Gela.Relocatable_Arrays.Index;
      Index : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Allocate (Self.Compilation.Store, Token_Size);

      Ignore : constant Boolean := Gela.Mutables.Compilations.Dummy_Reference;
      pragma Unreferenced (Ignore);
      --  This is for suppress unreference warging for
      --  Gela.Mutables.Compilations package
   begin
      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         Index + Offset.Value,
         Gela.Lexical.Tokens.Token'Pos (Value));

      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         Index + Offset.Line,
         Gela.Relocatable_Arrays.Element (Line));

      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         Index + Offset.First,
         Gela.Relocatable_Arrays.Element (First));

      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         Index + Offset.Last,
         Gela.Relocatable_Arrays.Element (Last));

      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         Index + Offset.Separator,
         Gela.Relocatable_Arrays.Element (Separator));

      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         Index + Offset.Symbol,
         Gela.Types.Symbol'Pos (Symbol));

      Result := Gela.Types.Payload (Index);
   end Create;

   -----------
   -- First --
   -----------

   overriding function First
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Text_Index
   is
      use type Gela.Relocatable_Arrays.Index;

      Index : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Index (Payload);
      Value : constant Gela.Relocatable_Arrays.Element :=
        Gela.Relocatable_Arrays.Get
          (Self.Compilation.Store, Index + Offset.First);
   begin
      return Gela.Lexical.Text_Index'Val (Value);
   end First;

   ----------
   -- Last --
   ----------

   overriding function Last
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Text_Index
   is
      use type Gela.Relocatable_Arrays.Index;

      Index : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Index (Payload);
      Value : constant Gela.Relocatable_Arrays.Element :=
        Gela.Relocatable_Arrays.Get
          (Self.Compilation.Store, Index + Offset.Last);
   begin
      return Gela.Lexical.Text_Index'Val (Value);
   end Last;

   ----------
   -- Line --
   ----------

   overriding function Line
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Line_Index
   is
      use type Gela.Relocatable_Arrays.Index;

      Index : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Index (Payload);
      Value : constant Gela.Relocatable_Arrays.Element :=
        Gela.Relocatable_Arrays.Get
          (Self.Compilation.Store, Index + Offset.Line);
   begin
      return Gela.Lexical.Line_Index'Val (Value);
   end Line;

   ---------------
   -- Separator --
   ---------------

   overriding function Separator
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Text_Index
   is
      use type Gela.Relocatable_Arrays.Index;

      Index : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Index (Payload);
      Value : constant Gela.Relocatable_Arrays.Element :=
        Gela.Relocatable_Arrays.Get
          (Self.Compilation.Store, Index + Offset.Separator);
   begin
      return Gela.Lexical.Text_Index'Val (Value);
   end Separator;

   ----------
   -- Size --
   ----------

   overriding function Size
     (Self    : access Token;
      Payload : Gela.Types.Payload) return Natural
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Payload);
   begin
      return Natural (Token_Size);
   end Size;

   -----------
   -- Value --
   -----------

   overriding function Value
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Lexical.Tokens.Token
   is
      use type Gela.Relocatable_Arrays.Index;

      Index : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Index (Payload);
      Value : constant Gela.Relocatable_Arrays.Element :=
        Gela.Relocatable_Arrays.Get
          (Self.Compilation.Store, Index + Offset.Value);
   begin
      return Gela.Lexical.Tokens.Token'Val (Value);
   end Value;

end Gela.Mutables.Tokens;
