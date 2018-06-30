with Gela.Elements.Defining_Names;
with Gela.Elements.Full_Type_Declarations;
with Gela.Elements.Subtype_Marks;
with Gela.Lexical_Types;
with Gela.Types.Arrays;
with Gela.Types.Simple;
with Gela.Types.Untagged_Records;
with Gela.Types.Visitors;
with Gela.Type_Categories;
with Gela.Semantic_Types;

package Gela.Derived_Type_Views is
   pragma Preelaborate;

   type Type_View is new Gela.Type_Categories.Type_View
     and Gela.Types.Simple.Enumeration_Type
     and Gela.Types.Simple.Signed_Integer_Type
     and Gela.Types.Simple.Floating_Point_Type
     and Gela.Types.Simple.Object_Access_Type
     and Gela.Types.Simple.Subprogram_Access_Type
     and Gela.Types.Untagged_Records.Untagged_Record_Type
     and Gela.Types.Arrays.Array_Type with private;

   type Type_View_Access is access all Type_View'Class;

   function Create_Derived_Type
     (Parent   : not null Gela.Type_Categories.Type_View_Access;
      Decl     : Gela.Elements.Full_Type_Declarations
                   .Full_Type_Declaration_Access)
      return Gela.Type_Categories.Type_View_Access;

private

   type Type_View is new Gela.Type_Categories.Type_View
     and Gela.Types.Simple.Enumeration_Type
     and Gela.Types.Simple.Signed_Integer_Type
     and Gela.Types.Simple.Floating_Point_Type
     and Gela.Types.Simple.Object_Access_Type
     and Gela.Types.Simple.Subprogram_Access_Type
     and Gela.Types.Untagged_Records.Untagged_Record_Type
     and Gela.Types.Arrays.Array_Type with
   record
      Parent   : not null Gela.Type_Categories.Type_View_Access;
      Decl     : Gela.Elements.Full_Type_Declarations
        .Full_Type_Declaration_Access;
   end record;

   overriding function Category
     (Self : Type_View) return Gela.Type_Categories.Category_Kinds;

   overriding function Get_Discriminant
     (Self   : Type_View;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Elements.Defining_Names.Defining_Name_Access;

   overriding function Get_Component
     (Self   : Type_View;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Elements.Defining_Names.Defining_Name_Access;

   overriding function Get_Designated
     (Self   : Type_View)
      return Gela.Elements.Subtype_Marks.Subtype_Mark_Access;

   overriding function Index_Types
     (Self : Type_View) return Gela.Types.Simple.Discrete_Type_Array;

   overriding function Index_Types
     (Self : Type_View) return Gela.Semantic_Types.Type_Index_Array;

   overriding function Dimension (Self : Type_View) return Positive;

   overriding function Component_Type
     (Self : Type_View) return Gela.Semantic_Types.Type_Index;

   overriding function Is_The_Same_Type
     (Left  : Type_View;
      Right : Gela.Types.Type_View'Class) return Boolean;

   overriding function Is_Expected_Type
     (Self     : Type_View;
      Expected : not null Gela.Types.Type_View_Access) return Boolean;

   overriding procedure Visit
     (Self    : not null access Type_View;
      Visiter : in out Gela.Types.Visitors.Type_Visitor'Class);

   overriding function Is_Array (Self : Type_View) return Boolean;

   overriding function Is_Character (Self : Type_View) return Boolean;

   overriding function Is_Enumeration (Self : Type_View) return Boolean;

   overriding function Is_Floating_Point (Self : Type_View) return Boolean;

   overriding function Is_Modular_Integer (Self : Type_View) return Boolean;

   overriding function Is_Object_Access (Self : Type_View) return Boolean;

   overriding function Is_Record (Self : Type_View) return Boolean;

   overriding function Is_Signed_Integer (Self : Type_View) return Boolean;

   overriding function Is_Universal (Self : Type_View) return Boolean;

   overriding function Is_Root (Self : Type_View) return Boolean;

   overriding function Defining_Name (Self : Type_View)
     return Gela.Elements.Defining_Names.Defining_Name_Access;

end Gela.Derived_Type_Views;
