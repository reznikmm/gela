with Gela.Semantic_Types;

package Gela.Int.Expressions is
   pragma Preelaborate;

   type Expression is new Interpretation with private;

   function Create
     (Down            : Gela.Interpretations.Interpretation_Index_Array;
      Expression_Type : Gela.Semantic_Types.Type_Index;
      Expression_Flag : Gela.Interpretations.Expression_Flags)
      return Expression;

   function Expression_Type
     (Self : Expression)
      return Gela.Semantic_Types.Type_Index;

   function Expression_Flag
     (Self : Expression)
      return Gela.Interpretations.Expression_Flags;

private

   type Expression is new Interpretation with record
      Expression_Type : Gela.Semantic_Types.Type_Index;
      Expression_Flag : Gela.Interpretations.Expression_Flags;
   end record;

   overriding procedure Visit
     (Self    : Expression;
      Visiter : access Gela.Int.Visiters.Visiter'Class);

end Gela.Int.Expressions;
