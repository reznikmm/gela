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

package body Gela.Compilations.Mutables.Folded_Sets is

   subtype Symbol is Gela.Elements.Symbol_Tables.Symbol;
   use type Symbol;

   subtype Character_Symbol is Symbol range 0 .. 16#10_FFFF#;
   subtype Operator_Symbol is Symbol range
     Character_Symbol'Last + 1 .. Character_Symbol'Last + 19;
   subtype Identifier_Symbol is Symbol range
     Operator_Symbol'Last + 1 .. Symbol'Last;
   pragma Unreferenced (Identifier_Symbol);

--        An_And_Operator,                   --  and
--        An_Or_Operator,                    --  or
--        An_Xor_Operator,                   --  xor
--        A_Mod_Operator,                    --  mod
--        A_Rem_Operator,                    --  rem
--        An_Abs_Operator,                   --  abs
--  7     A_Not_Operator);                   --  not
   Map_1 : constant array (Wide_Wide_Character range '&' .. '>') of Symbol :=
     ('<' => Operator_Symbol'First + 1,
      '=' => Operator_Symbol'First + 2,
      '>' => Operator_Symbol'First + 3,
      '-' => Operator_Symbol'First + 4,
      '/' => Operator_Symbol'First + 5,
      '*' => Operator_Symbol'First + 6,
      '&' => Operator_Symbol'First + 7,
      '+' => Operator_Symbol'First + 8,
      others => 0);

   Map_2 : constant array (Wide_Wide_Character range '<' .. '>') of Symbol :=
     ('<' => Operator_Symbol'First + 9,
      '=' => Operator_Symbol'First + 10,
      '>' => Operator_Symbol'First + 11,
      others => 0);

   Power_Value : constant Symbol := Operator_Symbol'First + 12;

   Power_Image : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("""**""");
--        A_Less_Than_Operator,              --  <
--  12    An_Equal_Operator,                 --  =
--        A_Greater_Than_Operator,           --  >
--        A_Minus_Operator,                  --  -
--        A_Divide_Operator,                 --  /
--        A_Multiply_Operator,               --  *
--        A_Concatenate_Operator,            --  &
--        A_Unary_Plus_Operator,             --  +
--        A_Less_Than_Or_Equal_Operator,     --  <=
--        A_Greater_Than_Or_Equal_Operator,  --  >=
--        A_Not_Equal_Operator,              --  /=
--        An_Exponentiate_Operator,          --  **

   function To_Operator_Symbol
     (Text : League.Strings.Universal_String) return Operator_Symbol;

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Self   : in out Folded_Set;
      Value  : League.Strings.Universal_String;
      Result : out Gela.Elements.Symbol_Tables.Symbol)
   is
      pragma Unreferenced (Self);
      use type League.Characters.Universal_Character;
      use type League.Strings.Universal_String;

      Length : constant Natural := Value.Length;
      First  : constant Wide_Wide_Character :=
        Value.Element (1).To_Wide_Wide_Character;
      Char   : Wide_Wide_Character;
   begin
      if First = ''' then
         Result := Wide_Wide_Character'Pos
           (Value.Element (2).To_Wide_Wide_Character);

         if Result not in Character_Symbol then
            Result := 0;
         end if;
      elsif First /= '"' then
         null;
      elsif Length = 3 then  --  "#"
         Char := Value.Element (2).To_Wide_Wide_Character;

         if Char in Map_1'Range then
            Result := Map_1 (Char);
         else
            Result := 0;
         end if;
      elsif Length = 4 then -- "##"
         if Value.Element (3) = '=' then
            Char := Value.Element (2).To_Wide_Wide_Character;
            Result := Map_2 (Char);
         elsif Value = Power_Image then
            Result := Power_Value;
         else
            Result := To_Operator_Symbol (Value);
         end if;
      elsif Length = 5 then -- "###"
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

      Token : constant Gela.Lexical.Tokens.Token :=
        Gela.Lexical.Handler.To_Token
          (Text.Slice (2 .. Text.Length - 1).To_Lowercase);
   begin
      return 0;
   end To_Operator_Symbol;

end Gela.Compilations.Mutables.Folded_Sets;
