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

   Next_Offset : constant Gela.Relocatable_Arrays.Index :=
     Gela.Properties.Property_Index
       (Gela.Properties.Token, Gela.Properties.Next);

   Separator_Offset : constant Gela.Relocatable_Arrays.Index :=
     Gela.Properties.Property_Index
       (Gela.Properties.Token, Gela.Properties.Separator);

   Symbol_Offset : constant Gela.Relocatable_Arrays.Index :=
     Gela.Properties.Property_Index
       (Gela.Properties.Token, Gela.Properties.Symbol);

   Value_Offset : constant Gela.Relocatable_Arrays.Index :=
     Gela.Properties.Property_Index
       (Gela.Properties.Token, Gela.Properties.Value);

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
        Gela.Relocatable_Arrays.Last (Self.Compilation.Store) + 1;
   begin
      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         Index + Value_Offset,
         Gela.Lexical.Tokens.Token'Pos (Value));

      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         Index + Line_Offset,
         Gela.Relocatable_Arrays.Element (Line));

      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         Index + First_Offset,
         Gela.Relocatable_Arrays.Element (First));

      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         Index + Last_Offset,
         Gela.Relocatable_Arrays.Element (Last));

      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         Index + Separator_Offset,
         Gela.Relocatable_Arrays.Element (Separator));

      Gela.Relocatable_Arrays.Set
        (Self.Compilation.Store,
         Index + Symbol_Offset,
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
          (Self.Compilation.Store, Index + First_Offset);
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
          (Self.Compilation.Store, Index + Last_Offset);
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
          (Self.Compilation.Store, Index + Line_Offset);
   begin
      return Gela.Lexical.Line_Index'Val (Value);
   end Line;

   ----------------
   -- Next_Token --
   ----------------

   overriding function Next_Token
     (Self    : access Token;
      Payload : Gela.Types.Payload)
      return Gela.Types.Token
   is
      use type Gela.Relocatable_Arrays.Index;

      Index : constant Gela.Relocatable_Arrays.Index :=
        Gela.Relocatable_Arrays.Index (Payload);
      Value : constant Gela.Relocatable_Arrays.Element :=
        Gela.Relocatable_Arrays.Get
          (Self.Compilation.Store, Index + Next_Offset);
   begin
      return (Object  => Gela.Types.Token_Access (Self),
              Payload => Gela.Types.Payload'Val (Value));
   end Next_Token;

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
          (Self.Compilation.Store, Index + Separator_Offset);
   begin
      return Gela.Lexical.Text_Index'Val (Value);
   end Separator;

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
          (Self.Compilation.Store, Index + Value_Offset);
   begin
      return Gela.Lexical.Tokens.Token'Val (Value);
   end Value;

end Gela.Compilations.Mutables.Tokens;
