with Gela.Lexical_Types;

package Gela.Int.Symbols is
   pragma Preelaborate;

   type Symbol is new Interpretation with private;

   function Create
     (Down  : Gela.Interpretations.Interpretation_Index_Array;
      Value : Gela.Lexical_Types.Symbol)
      return Symbol;

   function Get_Symbol
     (Self : Symbol)
      return Gela.Lexical_Types.Symbol;

private

   type Symbol is new Interpretation with record
      Symbol : Gela.Lexical_Types.Symbol;
   end record;

   overriding procedure Visit
     (Self    : Symbol;
      Visiter : access Gela.Int.Visiters.Visiter'Class);

end Gela.Int.Symbols;
