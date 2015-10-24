with Gela.Int.Visiters;

package body Gela.Int.Categories is

   ------------
   -- Create --
   ------------

   function Create
     (Down  : Gela.Interpretations.Interpretation_Index_Array;
      Kinds : Gela.Types.Category_Kind_Set)
      return Category is
   begin
      return (Index  => 0,
              Length => Down'Length,
              Kinds  => Kinds,
              Down   => Down);
   end Create;

   -----------
   -- Kinds --
   -----------

   function Kinds
     (Self : Category)
      return Gela.Types.Category_Kind_Set is
   begin
      return Self.Kinds;
   end Kinds;

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
