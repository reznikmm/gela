with Gela.Semantic_Types;

package Gela.Int.Expressions is
   pragma Preelaborate;

   type Expression is new Interpretation with private;

   function Create
     (Down            : Gela.Interpretations.Interpretation_Index_Array;
      Expression_Type : Gela.Semantic_Types.Type_Index;
      Expression_Kind : Gela.Interpretations.Unknown_Auxiliary_Apply_Kinds)
      return Expression;

   function Expression_Type
     (Self : Expression)
      return Gela.Semantic_Types.Type_Index;

   function Expression_Kind
     (Self : Expression)
      return Gela.Interpretations.Unknown_Auxiliary_Apply_Kinds;

private

   type Expression is new Interpretation with record
      Expression_Type : Gela.Semantic_Types.Type_Index;
      Expression_Kind : Gela.Interpretations.Unknown_Auxiliary_Apply_Kinds;
   end record;

   overriding procedure Visit
     (Self    : Expression;
      Visiter : access Gela.Int.Visiters.Visiter'Class);

end Gela.Int.Expressions;
