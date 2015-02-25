with Gela.Type_Views;

package Gela.Int.Categories is
   pragma Preelaborate;

   type Category is new Interpretation with private;

   function Create
     (Down  : Gela.Interpretations.Interpretation_Index_Array;
      Kinds : Gela.Type_Views.Category_Kind_Set)
      return Category;

   function Kinds
     (Self : Category)
      return Gela.Type_Views.Category_Kind_Set;

private

   type Category is new Interpretation with record
      Kinds : Gela.Type_Views.Category_Kind_Set;
   end record;

   overriding procedure Visit
     (Self    : Category;
      Visiter : access Gela.Int.Visiters.Visiter'Class);

end Gela.Int.Categories;
