------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with Gela.Properties;

package body Gela.Compilations.Mutables.Tokens is

   First_Offset : constant Gela.Relocatable_Arrays.Index :=
     Gela.Properties.Property_Index
       (Gela.Properties.Token, Gela.Properties.First);

   Last_Offset : constant Gela.Relocatable_Arrays.Index :=
     Gela.Properties.Property_Index
       (Gela.Properties.Token, Gela.Properties.Last);

   Line_Offset : constant Gela.Relocatable_Arrays.Index :=
     Gela.Properties.Property_Index
       (Gela.Properties.Token, Gela.Properties.Line);

   Separator_Offset : constant Gela.Relocatable_Arrays.Index :=
     Gela.Properties.Property_Index
       (Gela.Properties.Token, Gela.Properties.Separator);

   Value_Offset : constant Gela.Relocatable_Arrays.Index :=
     Gela.Properties.Property_Index
       (Gela.Properties.Token, Gela.Properties.Value);

   ------------
   -- Create --
   ------------

   not overriding procedure Create
     (Self      : Token;
      Result    : out Gela.Elements.Payload;
      Value     : Gela.Lexical.Tokens.Token;
      Line      : Gela.Lexical.Line_Index;
      First     : Gela.Lexical.Text_Index;
      Last      : Gela.Lexical.Text_Index;
      Separator : Gela.Lexical.Text_Index)
   is
      use type Gela.Relocatable_Arrays.Index;

      Index : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Last (Self.Compilation.Store) + 1;
   begin
      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         Value_Offset,
         Gela.Lexical.Tokens.Token'Pos (Value));

      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         Line_Offset,
         Gela.Relocatable_Arrays.Element (Line));

      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         First_Offset,
         Gela.Relocatable_Arrays.Element (First));

      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         Last_Offset,
         Gela.Relocatable_Arrays.Element (Last));

      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         Separator_Offset,
         Gela.Relocatable_Arrays.Element (Separator));

      Result := Gela.Elements.Payload (Index);
   end Create;

   -----------
   -- First --
   -----------

   overriding function First
     (Self    : Token;
      Payload : Gela.Elements.Payload)
      return Gela.Lexical.Text_Index
   is
      use type Gela.Relocatable_Arrays.Index;

      Index : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Index (Payload);
      Value : constant Gela.Relocatable_Arrays.Element :=
        Gela.Relocatable_Arrays.Get
          (Self.Compilation.Store, Index + First_Offset);
   begin
      return Gela.Lexical.Text_Index'Val (Value);
   end First;

   ----------
   -- Last --
   ----------

   overriding function Last
     (Self    : Token;
      Payload : Gela.Elements.Payload)
      return Gela.Lexical.Text_Index
   is
      use type Gela.Relocatable_Arrays.Index;

      Index : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Index (Payload);
      Value : constant Gela.Relocatable_Arrays.Element :=
        Gela.Relocatable_Arrays.Get
          (Self.Compilation.Store, Index + Last_Offset);
   begin
      return Gela.Lexical.Text_Index'Val (Value);
   end Last;

   ----------
   -- Line --
   ----------

   overriding function Line
     (Self    : Token;
      Payload : Gela.Elements.Payload)
      return Gela.Lexical.Line_Index
   is
      use type Gela.Relocatable_Arrays.Index;

      Index : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Index (Payload);
      Value : constant Gela.Relocatable_Arrays.Element :=
        Gela.Relocatable_Arrays.Get
          (Self.Compilation.Store, Index + Line_Offset);
   begin
      return Gela.Lexical.Line_Index'Val (Value);
   end Line;

   ---------------
   -- Separator --
   ---------------

   overriding function Separator
     (Self    : Token;
      Payload : Gela.Elements.Payload)
      return Gela.Lexical.Text_Index
   is
      use type Gela.Relocatable_Arrays.Index;

      Index : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Index (Payload);
      Value : constant Gela.Relocatable_Arrays.Element :=
        Gela.Relocatable_Arrays.Get
          (Self.Compilation.Store, Index + Separator_Offset);
   begin
      return Gela.Lexical.Text_Index'Val (Value);
   end Separator;

   -----------
   -- Value --
   -----------

   overriding function Value
     (Self    : Token;
      Payload : Gela.Elements.Payload)
      return Gela.Lexical.Tokens.Token
   is
      use type Gela.Relocatable_Arrays.Index;

      Index : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Index (Payload);
      Value : constant Gela.Relocatable_Arrays.Element :=
        Gela.Relocatable_Arrays.Get
          (Self.Compilation.Store, Index + Value_Offset);
   begin
      return Gela.Lexical.Tokens.Token'Val (Value);
   end Value;

end Gela.Compilations.Mutables.Tokens;
