with Gela.Semantic_Types;

limited with Gela.Int.Visiters;

package Gela.Int.Expressions is
   pragma Preelaborate;

   type Expression is new Interpretation with private;

   function Create
     (Expression_Type : Gela.Semantic_Types.Type_Index;
      Save : Gela.Saves.Save_Access)
      return Expression;

   function Expression_Type
     (Self : Expression)
      return Gela.Semantic_Types.Type_Index;

private

   type Expression is new Interpretation with record
      Expression_Type : Gela.Semantic_Types.Type_Index;
      Save : Gela.Saves.Save_Access;
   end record;

   overriding procedure Visit
     (Self    : Expression;
      Visiter : access Gela.Int.Visiters.Visiter'Class);

   overriding function Save
     (Self : Expression) return Gela.Saves.Save_Access;

end Gela.Int.Expressions;
