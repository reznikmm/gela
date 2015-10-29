with Gela.Types;

package Gela.Type_Categories is
   pragma Preelaborate;

   type Category_Kinds is
     (A_Character,
      A_Boolean,
      An_Other_Enum,
      An_Universal_Integer,
      A_Signed_Integer,
      A_Modular_Integer,
      An_Universal_Real,
      A_Float_Point,
      An_Universal_Fixed,
      A_Ordinary_Fixed_Point,
      A_Decimal_Fixed_Point,
      A_Constant_Access,
      A_Variable_Access,
      A_Pool_Access,
      A_Procedure_Access,
      A_Function_Access,
      An_Universal_Access,
      A_String,
      An_Other_Array,
      A_Untagged_Record,
      A_Tagged,
      A_Task,
      A_Protected,
      A_Private,
      An_Incomplete);

--     type Category_Kind_Set is array (Category_Kinds) of Boolean with Pack;

   subtype Any_Integer_Type is Category_Kinds
     range An_Universal_Integer .. A_Modular_Integer;

   subtype Any_Real_Type is Category_Kinds
     range An_Universal_Real .. A_Decimal_Fixed_Point;

   type Type_View is interface and Gela.Types.Type_View;

   type Type_View_Access is access all Type_View'Class;
   for Type_View_Access'Storage_Size use 0;

   not overriding function Category
     (Self : Type_View) return Category_Kinds is abstract;

end Gela.Type_Categories;
