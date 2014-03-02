with Gela.Int.Visiters;

package body Gela.Int.Symbol_Interpretations is

   ------------
   -- Create --
   ------------

   function Create
     (Symbol : Gela.Lexical_Types.Symbol)
      return Symbol_Interpretation is
   begin
      return (Symbol => Symbol);
   end Create;

   ------------
   -- Symbol --
   ------------

   function Symbol
     (Self : Symbol_Interpretation) return Gela.Lexical_Types.Symbol is
   begin
      return Self.Symbol;
   end Symbol;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : Symbol_Interpretation;
      Visiter : access Gela.Int.Visiters.Visiter'Class) is
   begin
      Visiter.Symbol_Interpretation (Self);
   end Visit;

end Gela.Int.Symbol_Interpretations;
