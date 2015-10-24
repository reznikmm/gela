--  This package provides representation of types and their categories.

with Gela.Elements.Defining_Names;
limited with Gela.Elements.Subtype_Indications;
with Gela.Lexical_Types;
limited with Gela.Types.Visitors;

package Gela.Types is
   pragma Preelaborate;

   type Type_View is limited interface;
   type Type_View_Access is access all Type_View'Class;
   for Type_View_Access'Storage_Size use 0;

   function Assigned (Self : access Type_View'Class) return Boolean
     is (Self /= null);

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

   type Category_Kind_Set is array (Category_Kinds) of Boolean with Pack;

   subtype Any_Integer_Type is Category_Kinds
     range An_Universal_Integer .. A_Modular_Integer;

   subtype Any_Real_Type is Category_Kinds
     range An_Universal_Real .. A_Decimal_Fixed_Point;

   not overriding function Category
     (Self : Type_View) return Category_Kinds is abstract;

   not overriding function Get_Discriminant
     (Self   : Type_View;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Elements.Defining_Names.Defining_Name_Access is abstract;

   not overriding function Get_Component
     (Self   : Type_View;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Elements.Defining_Names.Defining_Name_Access is abstract;

   not overriding function Get_Designated
     (Self   : Type_View)
      return Gela.Elements.Subtype_Indications.Subtype_Indication_Access
        is abstract;

   not overriding function Is_Expected_Type
     (Self     : Type_View;
      Expected : not null Type_View_Access) return Boolean is abstract;
   --  Given Self as type of construct, Expected type as specific type T
   --  return True if type of construct is expected type T. See ARM 8.6 (22)

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

   not overriding function Is_Array (Self : Type_View) return Boolean
     is abstract;

   not overriding procedure Visit
     (Self    : not null access Type_View;
      Visiter : in out Gela.Types.Visitors.Type_Visitor'Class) is abstract;

end Gela.Types;
