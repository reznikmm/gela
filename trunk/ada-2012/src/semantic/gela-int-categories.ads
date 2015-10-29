package Gela.Int.Categories is
   pragma Preelaborate;

   type Category is new Interpretation with private;

   function Create
     (Down  : Gela.Interpretations.Interpretation_Index_Array;
      Match  : not null Gela.Interpretations.Type_Matcher_Access)
      return Category;

   function Match
     (Self : Category)
      return not null Gela.Interpretations.Type_Matcher_Access;

private

   type Category is new Interpretation with record
      Match  : not null Gela.Interpretations.Type_Matcher_Access;
   end record;

   overriding procedure Visit
     (Self    : Category;
      Visiter : access Gela.Int.Visiters.Visiter'Class);

end Gela.Int.Categories;
