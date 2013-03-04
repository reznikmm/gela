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

package body Gela.Compilations.Mutables.Folded_Sets is

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

   Power_Image : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("""**""");

   function To_Operator_Symbol
     (Text : League.Strings.Universal_String) return Operator_Symbol;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Self   : in out Folded_Set;
      Value  : League.Strings.Universal_String;
      Result : out Gela.Types.Symbol)
   is
      use type League.Characters.Universal_Character;
      use type League.Strings.Universal_String;

      Shift  :  constant := 16#0100_0000#;
      Length : constant Natural := Value.Length;
      First  : Wide_Wide_Character :=
        Value.Element (1).To_Wide_Wide_Character;
      Char   : Wide_Wide_Character;
   begin
      if First = ''' then  --  Character literal
         Result := Wide_Wide_Character'Pos
           (Value.Element (2).To_Wide_Wide_Character);
      elsif First /= '"' then  --  Identifier
         if First not in 'a' .. 'z' then
            First := Non_ASCII;
         end if;

         if Self.Last (First) = 0 then
            Self.Last (First) := Wide_Wide_Character'Pos (First) * Shift;
            Self.Symbols.Insert (Value, Self.Last (First));
            Result := Self.Last (First);
         else
            declare
               Pos : constant Symbol_Maps.Cursor := Self.Symbols.Find (Value);
            begin
               if Symbol_Maps.Has_Element (Pos) then
                  Result := Symbol_Maps.Element (Pos);
               else
                  Self.Last (First) := Self.Last (First) + 1;
                  Self.Symbols.Insert (Value, Self.Last (First));
                  Result := Self.Last (First);
               end if;
            end;
         end if;
      elsif Length = 3 then  --  String literal (one charater) "#"
         Char := Value.Element (2).To_Wide_Wide_Character;

         if Char in Map_1'Range then
            Result := Map_1 (Char);
         else
            Result := 0;
         end if;
      elsif Length = 4 then  --  String literal (two charaters) "##"
         if Value.Element (3) = '=' then
            Char := Value.Element (2).To_Wide_Wide_Character;
            Result := Map_2 (Char);
         elsif Value = Power_Image then
            Result := Power_Value;
         else  --  This could be "or"
            Result := To_Operator_Symbol (Value);
         end if;
      elsif Length = 5 then  --  String literal (three charaters) "###"
         Result := To_Operator_Symbol (Value);
      else
         Result := 0;
      end if;
   end Append;

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

end Gela.Compilations.Mutables.Folded_Sets;
