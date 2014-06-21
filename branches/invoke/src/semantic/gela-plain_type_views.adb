package body Gela.Plain_Type_Views is

   --------------
   -- Category --
   --------------

   overriding function Category
     (Self : Type_View)
      return Gela.Type_Views.Category_Kinds
   is
   begin
      return Self.Category;
   end Category;

   ------------
   -- Create --
   ------------

   function Create_Full_Type
     (Category : Gela.Type_Views.Category_Kinds;
      Decl     : Gela.Elements.Full_Type_Declarations
      .Full_Type_Declaration_Access)
      return Gela.Type_Views.Type_View_Access
   is
      Value : constant Type_View_Access :=
        new Type_View'(Category => Category, Decl => Decl);
   begin
      return Gela.Type_Views.Type_View_Access (Value);
   end Create_Full_Type;

end Gela.Plain_Type_Views;
