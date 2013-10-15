------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.Characters;

with Gela.Lexical.Handler;
with Gela.Lexical.Tokens;

package body Gela.Mutables.Symbol_Sets is

   subtype Symbol is Gela.Types.Symbol;
   use type Symbol;

   use Gela.Lexical.Tokens;

   subtype Operator_Symbol   is Symbol range 0 .. 19;
   subtype Character_Symbol  is Symbol range 32 .. 16#10_FFFF#;
   subtype Identifier_Symbol is Symbol range
     Character_Symbol'Last + 1 .. Symbol'Last;
   pragma Unreferenced (Identifier_Symbol);

   Map_1 : constant array (Wide_Wide_Character range '&' .. '>') of
     Operator_Symbol :=
     ('<' => Token'Pos (Less_Token),
      '=' => Token'Pos (Equal_Token),
      '>' => Token'Pos (Greater_Token),
      '-' => Token'Pos (Hyphen_Token),
      '/' => Token'Pos (Slash_Token),
      '*' => Token'Pos (Star_Token),
      '&' => Token'Pos (Ampersand_Token),
      '+' => Token'Pos (Plus_Token),
      others => 0);

   Map_2 : constant array (Wide_Wide_Character range '/' .. '>') of
     Operator_Symbol :=
     ('<' => Token'Pos (Less_Or_Equal_Token),
      '/' => Token'Pos (Inequality_Token),
      '>' => Token'Pos (Greater_Or_Equal_Token),
      others => 0);

   Power_Value : constant Operator_Symbol := Token'Pos (Double_Star_Token);

   function To_Operator_Symbol
     (Text : League.Strings.Universal_String) return Operator_Symbol;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Self   : in out Symbol_Set;
      Value  : League.Strings.Universal_String;
      Result : out Gela.Types.Symbol)
   is
      use type League.Characters.Universal_Character;
      use type League.Strings.Universal_String;

      Shift  :  constant := 16#0100_0000#;
      First  : Wide_Wide_Character;
   begin
      Result := Self.Get (Value);

      if Result /= 0 then
         return;
      end if;

      First := Value.Element (1).To_Wide_Wide_Character;

      if First /= '"' then  --  Identifier
         if First not in 'a' .. 'z' then
            First := Non_ASCII;
         end if;

         if Self.Last (First) = 0 then
            Self.Last (First) := Wide_Wide_Character'Pos (First) * Shift;
            Self.Symbols.Insert (Value, Self.Last (First));
            Self.Revert.Insert (Self.Last (First), Value);
            Result := Self.Last (First);
         else
            Self.Last (First) := Self.Last (First) + 1;
            Self.Symbols.Insert (Value, Self.Last (First));
            Self.Revert.Insert (Self.Last (First), Value);
            Result := Self.Last (First);
         end if;
      end if;
   end Append;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Self   : in out Symbol_Set;
      Value  : League.Strings.Universal_String)
      return Gela.Types.Symbol
   is
      use type League.Characters.Universal_Character;
      use type League.Strings.Universal_String;

      Result : Gela.Types.Symbol := 0;
      Length : constant Natural := Value.Length;
      First  : constant Wide_Wide_Character :=
        Value.Element (1).To_Wide_Wide_Character;
      Char   : Wide_Wide_Character;
   begin
      if First = ''' then  --  Character literal
         Result := Wide_Wide_Character'Pos
           (Value.Element (2).To_Wide_Wide_Character);
      elsif First /= '"' then  --  Identifier
         declare
            Pos : constant Symbol_Maps.Cursor := Self.Symbols.Find (Value);
         begin
            if Symbol_Maps.Has_Element (Pos) then
               Result := Symbol_Maps.Element (Pos);
            end if;
         end;
      elsif Length = 3 then  --  String literal (one charater) "#"
         Char := Value.Element (2).To_Wide_Wide_Character;

         if Char in Map_1'Range then
            Result := Map_1 (Char);
         end if;
      elsif Length = 4 then  --  String literal (two charaters) "##"
         if Value.Element (3) = '=' then
            Char := Value.Element (2).To_Wide_Wide_Character;
            if Char in Map_2'Range then
               Result := Map_2 (Char);
            end if;
         elsif Value.Element (2) = '*' and Value.Element (3) = '*' then
            Result := Power_Value;
         else  --  This could be "or"
            Result := To_Operator_Symbol (Value);
         end if;
      elsif Length = 5 then  --  String literal (three charaters) "###"
         Result := To_Operator_Symbol (Value);
      end if;

      return Result;
   end Get;

   ----------
   -- Hash --
   ----------

   function Hash (X : Gela.Types.Symbol) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (X);
   end Hash;

   ----------
   -- Join --
   ----------

   overriding procedure Join
     (Self     : in out Symbol_Set;
      Prefix   : Gela.Types.Symbol;
      Selector : Gela.Types.Symbol;
      Result   : out Gela.Types.Symbol)
   is
      use type League.Strings.Universal_String;
      Image : constant League.Strings.Universal_String :=
        Self.Value (Prefix) & "." & Self.Value (Selector);
   begin
      Self.Append (Image, Result);
   end Join;

   ------------
   -- Prefix --
   ------------

   overriding function Prefix
     (Self   : in out Symbol_Set;
      Name   : Gela.Types.Symbol) return Gela.Types.Symbol
   is
      Image  : constant League.Strings.Universal_String := Self.Value (Name);
      Text   : constant Wide_Wide_String := Image.To_Wide_Wide_String;
      Point  : Natural := 0;
      Result : Gela.Types.Symbol := 0;
   begin
      for J in reverse Text'Range loop
         if Text (J) = '.' then
            Point := J;
            exit;
         end if;
      end loop;

      if Point /= 0 then
         Self.Append
           (League.Strings.To_Universal_String (Text (1 .. Point - 1)),
            Result);
      end if;

      return Result;
   end Prefix;

   ------------------------
   -- To_Operator_Symbol --
   ------------------------

   function To_Operator_Symbol
     (Text : League.Strings.Universal_String) return Operator_Symbol
   is
      --  Drop quote characters in "and"
      Word : constant League.Strings.Universal_String :=
        Text.Slice (2, Text.Length - 1);
      Value : constant Token := Gela.Lexical.Handler.Unfolded_To_Token (Word);
   begin
      if Value in Keyword_Operator_Token then
         return Token'Pos (Value);
      else
         return 0;
      end if;
   end To_Operator_Symbol;

   -----------
   -- Value --
   -----------

   overriding function Value
     (Self   : in out Symbol_Set;
      Name   : Symbol) return League.Strings.Universal_String
   is
      Result : League.Strings.Universal_String;

      A1 : constant array (Symbol range 1 .. 8) of Wide_Wide_String (1 .. 3) :=
        ("""<""", """=""", """>""", """-""",
         """/""", """*""", """&""", """+""");
      A2 : constant array (Symbol range 9 .. 13) of Wide_Wide_String (1 .. 4)
        := ("""<=""", """>=""", """/=""", """**""", """or""");
      A3 : constant array (Symbol range 14 .. 19) of Wide_Wide_String (1 .. 5)
        := ("""and""", """xor""", """mod""", """rem""", """abs""", """not""");

      Pos : Revert_Maps.Cursor;
   begin
      case Name is
         when Operator_Symbol =>
            Pos := Self.Revert.Find (Name);

            if Revert_Maps.Has_Element (Pos) then
               Result := Revert_Maps.Element (Pos);
               return Result;
            end if;

            case Name is
               when A1'Range =>
                  Result := League.Strings.To_Universal_String (A1 (Name));
               when A2'Range =>
                  Result := League.Strings.To_Universal_String (A2 (Name));
               when A3'Range =>
                  Result := League.Strings.To_Universal_String (A3 (Name));
               when others =>
                  null;
            end case;

            Self.Revert.Insert (Name, Result);
         when Character_Symbol =>
            Result.Append ("'");
            Result.Append (Wide_Wide_Character'Val (Name));
            Result.Append ("'");
         when others =>
            Result := Self.Revert.Element (Name);
      end case;

      return Result;
   end Value;

end Gela.Mutables.Symbol_Sets;
