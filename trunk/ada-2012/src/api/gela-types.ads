--  This package provides representation of types and their categories.

limited with Gela.Types.Visitors;

package Gela.Types is
   pragma Preelaborate;

   type Type_View is limited interface;
   type Type_View_Access is access all Type_View'Class;
   for Type_View_Access'Storage_Size use 0;

   function Assigned (Self : access Type_View'Class) return Boolean
     is (Self /= null);

   procedure Visit_If_Assigned
     (Self    : access Type_View'Class;
      Visiter : in out Gela.Types.Visitors.Type_Visitor'Class);

   not overriding function Is_Expected_Type
     (Self     : Type_View;
      Expected : not null Type_View_Access) return Boolean is abstract;
   --  Given Self as type of construct, Expected type as specific type T
   --  return True if type of construct is expected type T. See ARM 8.6 (22)

--     function Is_Elementary           (Self : Abstract_Type) return Boolean;
--     function Is_Scalar               (Self : Abstract_Type) return Boolean;
--     function Is_Discrete             (Self : Abstract_Type) return Boolean;
--     function Is_Enumeration          (Self : Abstract_Type) return Boolean;
--     function Is_Boolean              (Self : Abstract_Type) return Boolean;
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
--     function Is_Untagged_Record      (Self : Abstract_Type) return Boolean;
--     function Is_Tagged               (Self : Abstract_Type) return Boolean;
--     function Is_Task                 (Self : Abstract_Type) return Boolean;
--     function Is_Protected            (Self : Abstract_Type) return Boolean;
--     function Is_Access               (Self : Abstract_Type) return Boolean;
--     function Is_Composite            (Self : Abstract_Type) return Boolean;
--     function Is_Incomplete           (Self : Abstract_Type) return Boolean;
--
--     function Is_Array
--       (Self   : Abstract_Type;
--        Length : Positive) return Boolean;


   not overriding function Is_Array (Self : Type_View) return Boolean
     is abstract;

   not overriding function Is_Character (Self : Type_View) return Boolean
     is abstract;

   not overriding function Is_Enumeration (Self : Type_View) return Boolean
     is abstract;

   not overriding function Is_Floating_Point (Self : Type_View) return Boolean
     is abstract;

   not overriding function Is_Modular_Integer (Self : Type_View) return Boolean
     is abstract;

   not overriding function Is_Object_Access (Self : Type_View) return Boolean
     is abstract;

   not overriding function Is_Record (Self : Type_View) return Boolean
     is abstract;

   not overriding function Is_Signed_Integer (Self : Type_View) return Boolean
     is abstract;

   not overriding function Is_Universal (Self : Type_View) return Boolean
     is abstract;

   function Is_Discrete (Self : Type_View'Class) return Boolean;
   function Is_Integer  (Self : Type_View'Class) return Boolean;
   function Is_Real     (Self : Type_View'Class) return Boolean;
   function Is_Numeric  (Self : Type_View'Class) return Boolean;

   not overriding procedure Visit
     (Self    : not null access Type_View;
      Visiter : in out Gela.Types.Visitors.Type_Visitor'Class) is abstract;

end Gela.Types;
