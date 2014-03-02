with Gela.Lexical_Types;

limited with Gela.Int.Visiters;

package Gela.Int.Symbol_Interpretations is
   pragma Preelaborate;

   type Symbol_Interpretation is new Interpretation with private;

   function Create
     (Symbol : Gela.Lexical_Types.Symbol)
      return Symbol_Interpretation;

   function Symbol
     (Self : Symbol_Interpretation) return Gela.Lexical_Types.Symbol;

private

   type Symbol_Interpretation is new Interpretation with record
      Symbol : Gela.Lexical_Types.Symbol;
   end record;

   overriding procedure Visit
     (Self    : Symbol_Interpretation;
      Visiter : access Gela.Int.Visiters.Visiter'Class);

end Gela.Int.Symbol_Interpretations;
