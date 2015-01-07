with Gela.Elements.Defining_Names;
with Gela.Interpretations;

package body Gela.Nodes.Fixed_Operator_Symbols is

   --------------------------
   -- String_Literal_Token --
   --------------------------

   overriding function String_Literal_Token
     (Self    : Operator_Symbol)
      return Gela.Lexical_Types.Token_Count
   is
   begin
      return Self.Operator_Symbol_Token;
   end String_Literal_Token;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : access Operator_Symbol;
      Visiter : in out Gela.Element_Visiters.Visiter'Class)
   is
      use type Gela.Interpretations.Interpretation_Index;

      Name : constant Gela.Elements.Defining_Names.Defining_Name_Access :=
        Self.Defining_Name;
   begin
      if Self.Down = 0 or else Name.Assigned then
         Visiter.Operator_Symbol (Self);
      else
         Visiter.String_Literal (Self);
      end if;
   end Visit;

end Gela.Nodes.Fixed_Operator_Symbols;
