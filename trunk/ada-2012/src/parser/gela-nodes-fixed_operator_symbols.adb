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
      use type Gela.Lexical_Types.Symbol;
   begin
      if Self.Full_Name = Gela.Lexical_Types.No_Symbol then
         Visiter.String_Literal (Self);
      else
         Visiter.Operator_Symbol (Self);
      end if;
   end Visit;

end Gela.Nodes.Fixed_Operator_Symbols;
