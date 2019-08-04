--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements;

package Program.Nodes is

   pragma Pure (Program.Nodes);

   type Node is abstract limited new Program.Elements.Element with private;

private

   type Node is abstract limited new Program.Elements.Element with record
      Enclosing_Element : Program.Elements.Element_Access;
   end record;

   procedure Set_Enclosing_Element
     (Self  : access Program.Elements.Element'Class;
      Value : access Program.Elements.Element'Class);

   overriding function Enclosing_Element
     (Self : Node) return Program.Elements.Element_Access;

   overriding function Is_Part_Of_Implicit (Self : Node) return Boolean;

   overriding function Is_Part_Of_Inherited (Self : Node) return Boolean;

   overriding function Is_Part_Of_Instance (Self : Node) return Boolean;

   overriding function Is_Pragma (Self : Node) return Boolean;

   overriding function Is_Defining_Name (Self : Node) return Boolean;

   overriding function Is_Defining_Identifier (Self : Node) return Boolean;

   overriding function Is_Defining_Character_Literal
     (Self : Node) return Boolean;

   overriding function Is_Defining_Operator_Symbol
     (Self : Node) return Boolean;

   overriding function Is_Defining_Expanded_Name (Self : Node) return Boolean;

   overriding function Is_Declaration (Self : Node) return Boolean;

   overriding function Is_Type_Declaration (Self : Node) return Boolean;

   overriding function Is_Task_Type_Declaration (Self : Node) return Boolean;

   overriding function Is_Protected_Type_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Subtype_Declaration (Self : Node) return Boolean;

   overriding function Is_Object_Declaration (Self : Node) return Boolean;

   overriding function Is_Single_Task_Declaration (Self : Node) return Boolean;

   overriding function Is_Single_Protected_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Number_Declaration (Self : Node) return Boolean;

   overriding function Is_Enumeration_Literal_Specification
     (Self : Node) return Boolean;

   overriding function Is_Discriminant_Specification
     (Self : Node) return Boolean;

   overriding function Is_Component_Declaration (Self : Node) return Boolean;

   overriding function Is_Loop_Parameter_Specification
     (Self : Node) return Boolean;

   overriding function Is_Generalized_Iterator_Specification
     (Self : Node) return Boolean;

   overriding function Is_Element_Iterator_Specification
     (Self : Node) return Boolean;

   overriding function Is_Procedure_Declaration (Self : Node) return Boolean;

   overriding function Is_Function_Declaration (Self : Node) return Boolean;

   overriding function Is_Parameter_Specification (Self : Node) return Boolean;

   overriding function Is_Procedure_Body_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Function_Body_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Return_Object_Specification
     (Self : Node) return Boolean;

   overriding function Is_Package_Declaration (Self : Node) return Boolean;

   overriding function Is_Package_Body_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Object_Renaming_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Exception_Renaming_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Procedure_Renaming_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Function_Renaming_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Package_Renaming_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Generic_Package_Renaming_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Generic_Procedure_Renaming_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Generic_Function_Renaming_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Task_Body_Declaration (Self : Node) return Boolean;

   overriding function Is_Protected_Body_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Entry_Declaration (Self : Node) return Boolean;

   overriding function Is_Entry_Body_Declaration (Self : Node) return Boolean;

   overriding function Is_Entry_Index_Specification
     (Self : Node) return Boolean;

   overriding function Is_Procedure_Body_Stub (Self : Node) return Boolean;

   overriding function Is_Function_Body_Stub (Self : Node) return Boolean;

   overriding function Is_Package_Body_Stub (Self : Node) return Boolean;

   overriding function Is_Task_Body_Stub (Self : Node) return Boolean;

   overriding function Is_Protected_Body_Stub (Self : Node) return Boolean;

   overriding function Is_Exception_Declaration (Self : Node) return Boolean;

   overriding function Is_Choice_Parameter_Specification
     (Self : Node) return Boolean;

   overriding function Is_Generic_Package_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Generic_Procedure_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Generic_Function_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Package_Instantiation (Self : Node) return Boolean;

   overriding function Is_Procedure_Instantiation (Self : Node) return Boolean;

   overriding function Is_Function_Instantiation (Self : Node) return Boolean;

   overriding function Is_Formal_Object_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Formal_Type_Declaration (Self : Node) return Boolean;

   overriding function Is_Formal_Procedure_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Formal_Function_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Formal_Package_Declaration
     (Self : Node) return Boolean;

   overriding function Is_Definition (Self : Node) return Boolean;

   overriding function Is_Type_Definition (Self : Node) return Boolean;

   overriding function Is_Subtype_Indication (Self : Node) return Boolean;

   overriding function Is_Constraint (Self : Node) return Boolean;

   overriding function Is_Component_Definition (Self : Node) return Boolean;

   overriding function Is_Discrete_Range (Self : Node) return Boolean;

   overriding function Is_Discrete_Subtype_Indication
     (Self : Node) return Boolean;

   overriding function Is_Discrete_Range_Attribute_Reference
     (Self : Node) return Boolean;

   overriding function Is_Discrete_Simple_Expression_Range
     (Self : Node) return Boolean;

   overriding function Is_Unknown_Discriminant_Part
     (Self : Node) return Boolean;

   overriding function Is_Known_Discriminant_Part (Self : Node) return Boolean;

   overriding function Is_Record_Definition (Self : Node) return Boolean;

   overriding function Is_Null_Component (Self : Node) return Boolean;

   overriding function Is_Variant_Part (Self : Node) return Boolean;

   overriding function Is_Variant (Self : Node) return Boolean;

   overriding function Is_Others_Choice (Self : Node) return Boolean;

   overriding function Is_Anonymous_Access_Definition
     (Self : Node) return Boolean;

   overriding function Is_Anonymous_Access_To_Object
     (Self : Node) return Boolean;

   overriding function Is_Anonymous_Access_To_Procedure
     (Self : Node) return Boolean;

   overriding function Is_Anonymous_Access_To_Function
     (Self : Node) return Boolean;

   overriding function Is_Private_Type_Definition (Self : Node) return Boolean;

   overriding function Is_Private_Extension_Definition
     (Self : Node) return Boolean;

   overriding function Is_Incomplete_Type_Definition
     (Self : Node) return Boolean;

   overriding function Is_Task_Definition (Self : Node) return Boolean;

   overriding function Is_Protected_Definition (Self : Node) return Boolean;

   overriding function Is_Formal_Type_Definition (Self : Node) return Boolean;

   overriding function Is_Aspect_Specification (Self : Node) return Boolean;

   overriding function Is_Real_Range_Specification
     (Self : Node) return Boolean;

   overriding function Is_Expression (Self : Node) return Boolean;

   overriding function Is_Numeric_Literal (Self : Node) return Boolean;

   overriding function Is_String_Literal (Self : Node) return Boolean;

   overriding function Is_Identifier (Self : Node) return Boolean;

   overriding function Is_Operator_Symbol (Self : Node) return Boolean;

   overriding function Is_Character_Literal (Self : Node) return Boolean;

   overriding function Is_Explicit_Dereference (Self : Node) return Boolean;

   overriding function Is_Function_Call (Self : Node) return Boolean;

   overriding function Is_Indexed_Component (Self : Node) return Boolean;

   overriding function Is_Slice (Self : Node) return Boolean;

   overriding function Is_Selected_Component (Self : Node) return Boolean;

   overriding function Is_Attribute_Reference (Self : Node) return Boolean;

   overriding function Is_Record_Aggregate (Self : Node) return Boolean;

   overriding function Is_Extension_Aggregate (Self : Node) return Boolean;

   overriding function Is_Array_Aggregate (Self : Node) return Boolean;

   overriding function Is_Short_Circuit_Operation (Self : Node) return Boolean;

   overriding function Is_Membership_Test (Self : Node) return Boolean;

   overriding function Is_Null_Literal (Self : Node) return Boolean;

   overriding function Is_Parenthesized_Expression
     (Self : Node) return Boolean;

   overriding function Is_Raise_Expression (Self : Node) return Boolean;

   overriding function Is_Type_Conversion (Self : Node) return Boolean;

   overriding function Is_Qualified_Expression (Self : Node) return Boolean;

   overriding function Is_Allocator (Self : Node) return Boolean;

   overriding function Is_Case_Expression (Self : Node) return Boolean;

   overriding function Is_If_Expression (Self : Node) return Boolean;

   overriding function Is_Quantified_Expression (Self : Node) return Boolean;

   overriding function Is_Association (Self : Node) return Boolean;

   overriding function Is_Discriminant_Association
     (Self : Node) return Boolean;

   overriding function Is_Record_Component_Association
     (Self : Node) return Boolean;

   overriding function Is_Array_Component_Association
     (Self : Node) return Boolean;

   overriding function Is_Parameter_Association (Self : Node) return Boolean;

   overriding function Is_Formal_Package_Association
     (Self : Node) return Boolean;

   overriding function Is_Statement (Self : Node) return Boolean;

   overriding function Is_Null_Statement (Self : Node) return Boolean;

   overriding function Is_Assignment_Statement (Self : Node) return Boolean;

   overriding function Is_If_Statement (Self : Node) return Boolean;

   overriding function Is_Case_Statement (Self : Node) return Boolean;

   overriding function Is_Loop_Statement (Self : Node) return Boolean;

   overriding function Is_While_Loop_Statement (Self : Node) return Boolean;

   overriding function Is_For_Loop_Statement (Self : Node) return Boolean;

   overriding function Is_Block_Statement (Self : Node) return Boolean;

   overriding function Is_Exit_Statement (Self : Node) return Boolean;

   overriding function Is_Goto_Statement (Self : Node) return Boolean;

   overriding function Is_Call_Statement (Self : Node) return Boolean;

   overriding function Is_Simple_Return_Statement (Self : Node) return Boolean;

   overriding function Is_Extended_Return_Statement
     (Self : Node) return Boolean;

   overriding function Is_Accept_Statement (Self : Node) return Boolean;

   overriding function Is_Requeue_Statement (Self : Node) return Boolean;

   overriding function Is_Delay_Statement (Self : Node) return Boolean;

   overriding function Is_Terminate_Alternative_Statement
     (Self : Node) return Boolean;

   overriding function Is_Select_Statement (Self : Node) return Boolean;

   overriding function Is_Abort_Statement (Self : Node) return Boolean;

   overriding function Is_Raise_Statement (Self : Node) return Boolean;

   overriding function Is_Code_Statement (Self : Node) return Boolean;

   overriding function Is_Path (Self : Node) return Boolean;

   overriding function Is_Elsif_Path (Self : Node) return Boolean;

   overriding function Is_Case_Path (Self : Node) return Boolean;

   overriding function Is_Select_Path (Self : Node) return Boolean;

   overriding function Is_Case_Expression_Path (Self : Node) return Boolean;

   overriding function Is_Elsif_Expression_Path (Self : Node) return Boolean;

   overriding function Is_Clause (Self : Node) return Boolean;

   overriding function Is_Use_Clause (Self : Node) return Boolean;

   overriding function Is_With_Clause (Self : Node) return Boolean;

   overriding function Is_Representation_Clause (Self : Node) return Boolean;

   overriding function Is_Component_Clause (Self : Node) return Boolean;

   overriding function Is_Derived_Type (Self : Node) return Boolean;

   overriding function Is_Derived_Record_Extension
     (Self : Node) return Boolean;

   overriding function Is_Enumeration_Type (Self : Node) return Boolean;

   overriding function Is_Signed_Integer_Type (Self : Node) return Boolean;

   overriding function Is_Modular_Type (Self : Node) return Boolean;

   overriding function Is_Root_Type (Self : Node) return Boolean;

   overriding function Is_Floating_Point_Type (Self : Node) return Boolean;

   overriding function Is_Ordinary_Fixed_Point_Type
     (Self : Node) return Boolean;

   overriding function Is_Decimal_Fixed_Point_Type
     (Self : Node) return Boolean;

   overriding function Is_Unconstrained_Array_Type
     (Self : Node) return Boolean;

   overriding function Is_Constrained_Array_Type (Self : Node) return Boolean;

   overriding function Is_Record_Type (Self : Node) return Boolean;

   overriding function Is_Interface_Type (Self : Node) return Boolean;

   overriding function Is_Object_Access_Type (Self : Node) return Boolean;

   overriding function Is_Procedure_Access_Type (Self : Node) return Boolean;

   overriding function Is_Function_Access_Type (Self : Node) return Boolean;

   overriding function Is_Formal_Private_Type_Definition
     (Self : Node) return Boolean;

   overriding function Is_Formal_Derived_Type_Definition
     (Self : Node) return Boolean;

   overriding function Is_Formal_Discrete_Type_Definition
     (Self : Node) return Boolean;

   overriding function Is_Formal_Signed_Integer_Type_Definition
     (Self : Node) return Boolean;

   overriding function Is_Formal_Modular_Type_Definition
     (Self : Node) return Boolean;

   overriding function Is_Formal_Floating_Point_Definition
     (Self : Node) return Boolean;

   overriding function Is_Formal_Ordinary_Fixed_Point_Definition
     (Self : Node) return Boolean;

   overriding function Is_Formal_Decimal_Fixed_Point_Definition
     (Self : Node) return Boolean;

   overriding function Is_Formal_Unconstrained_Array_Type
     (Self : Node) return Boolean;

   overriding function Is_Formal_Constrained_Array_Type
     (Self : Node) return Boolean;

   overriding function Is_Formal_Access_Type (Self : Node) return Boolean;

   overriding function Is_Formal_Object_Access_Type
     (Self : Node) return Boolean;

   overriding function Is_Formal_Procedure_Access_Type
     (Self : Node) return Boolean;

   overriding function Is_Formal_Function_Access_Type
     (Self : Node) return Boolean;

   overriding function Is_Formal_Interface_Type (Self : Node) return Boolean;

   overriding function Is_Access_Type (Self : Node) return Boolean;

   overriding function Is_Range_Attribute_Reference
     (Self : Node) return Boolean;

   overriding function Is_Simple_Expression_Range (Self : Node) return Boolean;

   overriding function Is_Digits_Constraint (Self : Node) return Boolean;

   overriding function Is_Delta_Constraint (Self : Node) return Boolean;

   overriding function Is_Index_Constraint (Self : Node) return Boolean;

   overriding function Is_Discriminant_Constraint (Self : Node) return Boolean;

   overriding function Is_Attribute_Definition_Clause
     (Self : Node) return Boolean;

   overriding function Is_Enumeration_Representation_Clause
     (Self : Node) return Boolean;

   overriding function Is_Record_Representation_Clause
     (Self : Node) return Boolean;

   overriding function Is_At_Clause (Self : Node) return Boolean;

   overriding function Is_Exception_Handler (Self : Node) return Boolean;

end Program.Nodes;
