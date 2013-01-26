separate (Gela.Grammars.Scanners)
procedure On_Accept
  (Self    : not null access Gela.Grammars.Scanner_Handlers.Handler'Class;
   Scanner : not null access Gela.Grammars.Scanners.Scanner'Class;
   Rule    : Gela.Grammars.Scanner_Types.Rule_Index;
   Token   : out Ag_Tokens.Token;
   Skip    : in out Boolean) is
begin
   case Rule is
      when 1 =>
         Self.Equal_Token (Scanner, Token, Skip);

      when 2 =>
         Self.Inherited_Token (Scanner, Token, Skip);

      when 3 =>
         Self.Synthesized_Token (Scanner, Token, Skip);

      when 4 =>
         Self.Attributes_Token (Scanner, Token, Skip);

      when 5 =>
         Self.Rules_Token (Scanner, Token, Skip);

      when 6 =>
         Self.Token_Token (Scanner, Token, Skip);

      when 7 =>
         Self.With_Token (Scanner, Token, Skip);

      when 8 =>
         Self.For_Token (Scanner, Token, Skip);

      when 9 =>
         Self.Identifier_Token (Scanner, Token, Skip);

      when 10 =>
         Self.Open_Rule_Token (Scanner, Token, Skip);

      when 11 =>
         Self.Rule_Body_Token (Scanner, Token, Skip);

      when 12 =>
         Self.Close_Rule_Token (Scanner, Token, Skip);

      when 13 =>
         Self.Semicolon_Token (Scanner, Token, Skip);

      when 14 =>
         Self.Open_Production_Name_Token (Scanner, Token, Skip);

      when 15 =>
         Self.Close_Production_Name_Token (Scanner, Token, Skip);

      when 16 =>
         Self.Open_Part_Name_Token (Scanner, Token, Skip);

      when 17 =>
         Self.Close_Part_Name_Token (Scanner, Token, Skip);

      when 18 =>
         Self.Open_List_Token (Scanner, Token, Skip);

      when 19 =>
         Self.Close_List_Token (Scanner, Token, Skip);

      when 20 =>
         Self.Open_Option_Token (Scanner, Token, Skip);

      when 21 =>
         Self.Close_Option_Token (Scanner, Token, Skip);

      when 22 =>
         Self.Colon_Token (Scanner, Token, Skip);

      when 23 =>
         Self.Or_Token (Scanner, Token, Skip);

      when 24 =>
         Self.Comma_Token (Scanner, Token, Skip);

      when 25 =>
         Self.Regexp_Token (Scanner, Token, Skip);

      when 26 =>
         Self.Spaces (Scanner, Token, Skip);

      when 27 =>
         Self.Comment (Scanner, Token, Skip);

      when 28 =>
         Self.New_Line (Scanner, Token, Skip);

      when others =>
         raise Constraint_Error;
   end case;
end On_Accept;