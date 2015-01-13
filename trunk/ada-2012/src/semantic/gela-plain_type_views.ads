with Gela.Elements.Defining_Names;
with Gela.Elements.Full_Type_Declarations;
with Gela.Lexical_Types;
with Gela.Type_Views;

package Gela.Plain_Type_Views is
   pragma Preelaborate;

   type Type_View is new Gela.Type_Views.Type_View with private;
   type Type_View_Access is access all Type_View'Class;

   function Create_Full_Type
     (Category : Gela.Type_Views.Category_Kinds;
      Decl     : Gela.Elements.Full_Type_Declarations
                   .Full_Type_Declaration_Access)
      return Gela.Type_Views.Type_View_Access;

private

   type Type_View is new Gela.Type_Views.Type_View with record
      Category : Gela.Type_Views.Category_Kinds;
      Decl     : Gela.Elements.Full_Type_Declarations
        .Full_Type_Declaration_Access;
   end record;

   overriding function Category
     (Self : Type_View) return Gela.Type_Views.Category_Kinds;

   overriding function Get_Discriminant
     (Self   : Type_View;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Elements.Defining_Names.Defining_Name_Access;

end Gela.Plain_Type_Views;
