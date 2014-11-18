--  This package provides representation of types and their categories.

package Gela.Type_Views is
   pragma Preelaborate;

   type Type_View is limited interface;
   type Type_View_Access is access all Type_View'Class;
   for Type_View_Access'Storage_Size use 0;

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

   not overriding function Category
     (Self : Type_View) return Category_Kinds is abstract;

--     function Is_Elementary           (Self : Abstract_Type) return Boolean;
--     function Is_Scalar               (Self : Abstract_Type) return Boolean;
--     function Is_Discrete             (Self : Abstract_Type) return Boolean;
--     function Is_Enumeration          (Self : Abstract_Type) return Boolean;
--     function Is_Character            (Self : Abstract_Type) return Boolean;
--     function Is_Boolean              (Self : Abstract_Type) return Boolean;
--     function Is_Signed_Integer       (Self : Abstract_Type) return Boolean;
--     function Is_Modular_Integer      (Self : Abstract_Type) return Boolean;
--     function Is_Float_Point          (Self : Abstract_Type) return Boolean;
--     function Is_Ordinary_Fixed_Point (Self : Abstract_Type) return Boolean;
--     function Is_Decimal_Fixed_Point  (Self : Abstract_Type) return Boolean;
--     function Is_Constant_Access      (Self : Abstract_Type) return Boolean;
--     function Is_Variable_Access      (Self : Abstract_Type) return Boolean;
--     function Is_Object_Access        (Self : Abstract_Type) return Boolean;
--     function Is_General_Access       (Self : Abstract_Type) return Boolean;
--     function Is_Procedure_Access     (Self : Abstract_Type) return Boolean;
--     function Is_Function_Access      (Self : Abstract_Type) return Boolean;
--     function Is_Subprogram_Access    (Self : Abstract_Type) return Boolean;
--     function Is_String               (Self : Abstract_Type) return Boolean;
--     function Is_Array                (Self : Abstract_Type) return Boolean;
--     function Is_Untagged_Record      (Self : Abstract_Type) return Boolean;
--     function Is_Tagged               (Self : Abstract_Type) return Boolean;
--     function Is_Task                 (Self : Abstract_Type) return Boolean;
--     function Is_Protected            (Self : Abstract_Type) return Boolean;
--     function Is_Integer              (Self : Abstract_Type) return Boolean;
--     function Is_Real                 (Self : Abstract_Type) return Boolean;
--     function Is_Fixed_Point          (Self : Abstract_Type) return Boolean;
--     function Is_Numeric              (Self : Abstract_Type) return Boolean;
--     function Is_Access               (Self : Abstract_Type) return Boolean;
--     function Is_Composite            (Self : Abstract_Type) return Boolean;
--     function Is_Universal            (Self : Abstract_Type) return Boolean;
--     function Is_Incomplete           (Self : Abstract_Type) return Boolean;
--
--     function Is_Array
--       (Self   : Abstract_Type;
--        Length : Positive) return Boolean;

end Gela.Type_Views;