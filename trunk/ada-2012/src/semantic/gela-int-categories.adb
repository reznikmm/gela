with Gela.Int.Visiters;

package body Gela.Int.Categories is

   ------------
   -- Create --
   ------------

   function Create
     (Down  : Gela.Interpretations.Interpretation_Index_Array;
      Match  : not null Gela.Interpretations.Type_Matcher_Access)
      return Category is
   begin
      return (Index  => 0,
              Length => Down'Length,
              Match  => Match,
              Down   => Down);
   end Create;

   -----------
   -- Match --
   -----------

   function Match
     (Self : Category)
      return not null Gela.Interpretations.Type_Matcher_Access is
   begin
      return Self.Match;
   end Match;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : Category;
      Visiter : access Gela.Int.Visiters.Visiter'Class) is
   begin
      Visiter.Expression_Category (Self);
   end Visit;

end Gela.Int.Categories;
