--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements;

package Program.Nodes is

   pragma Preelaborate;

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

   overriding function Is_Pragma_Element (Self : Node) return Boolean;

   overriding function Is_Defining_Name_Element (Self : Node) return Boolean;

   overriding function Is_Defining_Identifier_Element
     (Self : Node) return Boolean;

   overriding function Is_Defining_Character_Literal_Element
     (Self : Node) return Boolean;

   overriding function Is_Defining_Operator_Symbol_Element
     (Self : Node) return Boolean;

   overriding function Is_Defining_Expanded_Name_Element
     (Self : Node) return Boolean;

   overriding function Is_Declaration_Element (Self : Node) return Boolean;

   overriding function Is_Type_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Task_Type_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Protected_Type_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Subtype_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Object_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Single_Task_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Single_Protected_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Number_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Enumeration_Literal_Specification_Element
     (Self : Node) return Boolean;

   overriding function Is_Discriminant_Specification_Element
     (Self : Node) return Boolean;

   overriding function Is_Component_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Loop_Parameter_Specification_Element
     (Self : Node) return Boolean;

   overriding function Is_Generalized_Iterator_Specification_Element
     (Self : Node) return Boolean;

   overriding function Is_Element_Iterator_Specification_Element
     (Self : Node) return Boolean;

   overriding function Is_Procedure_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Function_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Parameter_Specification_Element
     (Self : Node) return Boolean;

   overriding function Is_Procedure_Body_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Function_Body_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Return_Object_Specification_Element
     (Self : Node) return Boolean;

   overriding function Is_Package_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Package_Body_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Object_Renaming_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Exception_Renaming_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Procedure_Renaming_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Function_Renaming_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Package_Renaming_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Generic_Package_Renaming_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Generic_Procedure_Renaming_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Generic_Function_Renaming_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Task_Body_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Protected_Body_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Entry_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Entry_Body_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Entry_Index_Specification_Element
     (Self : Node) return Boolean;

   overriding function Is_Procedure_Body_Stub_Element
     (Self : Node) return Boolean;

   overriding function Is_Function_Body_Stub_Element
     (Self : Node) return Boolean;

   overriding function Is_Package_Body_Stub_Element
     (Self : Node) return Boolean;

   overriding function Is_Task_Body_Stub_Element (Self : Node) return Boolean;

   overriding function Is_Protected_Body_Stub_Element
     (Self : Node) return Boolean;

   overriding function Is_Exception_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Choice_Parameter_Specification_Element
     (Self : Node) return Boolean;

   overriding function Is_Generic_Package_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Generic_Procedure_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Generic_Function_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Package_Instantiation_Element
     (Self : Node) return Boolean;

   overriding function Is_Procedure_Instantiation_Element
     (Self : Node) return Boolean;

   overriding function Is_Function_Instantiation_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Object_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Type_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Procedure_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Function_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Package_Declaration_Element
     (Self : Node) return Boolean;

   overriding function Is_Definition_Element (Self : Node) return Boolean;

   overriding function Is_Type_Definition_Element (Self : Node) return Boolean;

   overriding function Is_Subtype_Indication_Element
     (Self : Node) return Boolean;

   overriding function Is_Constraint_Element (Self : Node) return Boolean;

   overriding function Is_Component_Definition_Element
     (Self : Node) return Boolean;

   overriding function Is_Discrete_Range_Element (Self : Node) return Boolean;

   overriding function Is_Discrete_Subtype_Indication_Element
     (Self : Node) return Boolean;

   overriding function Is_Discrete_Range_Attribute_Reference_Element
     (Self : Node) return Boolean;

   overriding function Is_Discrete_Simple_Expression_Range_Element
     (Self : Node) return Boolean;

   overriding function Is_Unknown_Discriminant_Part_Element
     (Self : Node) return Boolean;

   overriding function Is_Known_Discriminant_Part_Element
     (Self : Node) return Boolean;

   overriding function Is_Record_Definition_Element
     (Self : Node) return Boolean;

   overriding function Is_Null_Component_Element (Self : Node) return Boolean;

   overriding function Is_Variant_Part_Element (Self : Node) return Boolean;

   overriding function Is_Variant_Element (Self : Node) return Boolean;

   overriding function Is_Others_Choice_Element (Self : Node) return Boolean;

   overriding function Is_Anonymous_Access_Definition_Element
     (Self : Node) return Boolean;

   overriding function Is_Anonymous_Access_To_Object_Element
     (Self : Node) return Boolean;

   overriding function Is_Anonymous_Access_To_Procedure_Element
     (Self : Node) return Boolean;

   overriding function Is_Anonymous_Access_To_Function_Element
     (Self : Node) return Boolean;

   overriding function Is_Private_Type_Definition_Element
     (Self : Node) return Boolean;

   overriding function Is_Private_Extension_Definition_Element
     (Self : Node) return Boolean;

   overriding function Is_Incomplete_Type_Definition_Element
     (Self : Node) return Boolean;

   overriding function Is_Task_Definition_Element (Self : Node) return Boolean;

   overriding function Is_Protected_Definition_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Type_Definition_Element
     (Self : Node) return Boolean;

   overriding function Is_Aspect_Specification_Element
     (Self : Node) return Boolean;

   overriding function Is_Real_Range_Specification_Element
     (Self : Node) return Boolean;

   overriding function Is_Expression_Element (Self : Node) return Boolean;

   overriding function Is_Numeric_Literal_Element (Self : Node) return Boolean;

   overriding function Is_String_Literal_Element (Self : Node) return Boolean;

   overriding function Is_Identifier_Element (Self : Node) return Boolean;

   overriding function Is_Operator_Symbol_Element (Self : Node) return Boolean;

   overriding function Is_Character_Literal_Element
     (Self : Node) return Boolean;

   overriding function Is_Explicit_Dereference_Element
     (Self : Node) return Boolean;

   overriding function Is_Function_Call_Element (Self : Node) return Boolean;

   overriding function Is_Infix_Operator_Element (Self : Node) return Boolean;

   overriding function Is_Indexed_Component_Element
     (Self : Node) return Boolean;

   overriding function Is_Slice_Element (Self : Node) return Boolean;

   overriding function Is_Selected_Component_Element
     (Self : Node) return Boolean;

   overriding function Is_Attribute_Reference_Element
     (Self : Node) return Boolean;

   overriding function Is_Record_Aggregate_Element
     (Self : Node) return Boolean;

   overriding function Is_Extension_Aggregate_Element
     (Self : Node) return Boolean;

   overriding function Is_Array_Aggregate_Element (Self : Node) return Boolean;

   overriding function Is_Short_Circuit_Operation_Element
     (Self : Node) return Boolean;

   overriding function Is_Membership_Test_Element (Self : Node) return Boolean;

   overriding function Is_Null_Literal_Element (Self : Node) return Boolean;

   overriding function Is_Parenthesized_Expression_Element
     (Self : Node) return Boolean;

   overriding function Is_Raise_Expression_Element
     (Self : Node) return Boolean;

   overriding function Is_Type_Conversion_Element (Self : Node) return Boolean;

   overriding function Is_Qualified_Expression_Element
     (Self : Node) return Boolean;

   overriding function Is_Allocator_Element (Self : Node) return Boolean;

   overriding function Is_Case_Expression_Element (Self : Node) return Boolean;

   overriding function Is_If_Expression_Element (Self : Node) return Boolean;

   overriding function Is_Quantified_Expression_Element
     (Self : Node) return Boolean;

   overriding function Is_Association_Element (Self : Node) return Boolean;

   overriding function Is_Discriminant_Association_Element
     (Self : Node) return Boolean;

   overriding function Is_Record_Component_Association_Element
     (Self : Node) return Boolean;

   overriding function Is_Array_Component_Association_Element
     (Self : Node) return Boolean;

   overriding function Is_Parameter_Association_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Package_Association_Element
     (Self : Node) return Boolean;

   overriding function Is_Statement_Element (Self : Node) return Boolean;

   overriding function Is_Null_Statement_Element (Self : Node) return Boolean;

   overriding function Is_Assignment_Statement_Element
     (Self : Node) return Boolean;

   overriding function Is_If_Statement_Element (Self : Node) return Boolean;

   overriding function Is_Case_Statement_Element (Self : Node) return Boolean;

   overriding function Is_Loop_Statement_Element (Self : Node) return Boolean;

   overriding function Is_While_Loop_Statement_Element
     (Self : Node) return Boolean;

   overriding function Is_For_Loop_Statement_Element
     (Self : Node) return Boolean;

   overriding function Is_Block_Statement_Element (Self : Node) return Boolean;

   overriding function Is_Exit_Statement_Element (Self : Node) return Boolean;

   overriding function Is_Goto_Statement_Element (Self : Node) return Boolean;

   overriding function Is_Call_Statement_Element (Self : Node) return Boolean;

   overriding function Is_Simple_Return_Statement_Element
     (Self : Node) return Boolean;

   overriding function Is_Extended_Return_Statement_Element
     (Self : Node) return Boolean;

   overriding function Is_Accept_Statement_Element
     (Self : Node) return Boolean;

   overriding function Is_Requeue_Statement_Element
     (Self : Node) return Boolean;

   overriding function Is_Delay_Statement_Element (Self : Node) return Boolean;

   overriding function Is_Terminate_Alternative_Statement_Element
     (Self : Node) return Boolean;

   overriding function Is_Select_Statement_Element
     (Self : Node) return Boolean;

   overriding function Is_Abort_Statement_Element (Self : Node) return Boolean;

   overriding function Is_Raise_Statement_Element (Self : Node) return Boolean;

   overriding function Is_Code_Statement_Element (Self : Node) return Boolean;

   overriding function Is_Path_Element (Self : Node) return Boolean;

   overriding function Is_Elsif_Path_Element (Self : Node) return Boolean;

   overriding function Is_Case_Path_Element (Self : Node) return Boolean;

   overriding function Is_Select_Path_Element (Self : Node) return Boolean;

   overriding function Is_Case_Expression_Path_Element
     (Self : Node) return Boolean;

   overriding function Is_Elsif_Expression_Path_Element
     (Self : Node) return Boolean;

   overriding function Is_Clause_Element (Self : Node) return Boolean;

   overriding function Is_Use_Clause_Element (Self : Node) return Boolean;

   overriding function Is_With_Clause_Element (Self : Node) return Boolean;

   overriding function Is_Representation_Clause_Element
     (Self : Node) return Boolean;

   overriding function Is_Component_Clause_Element
     (Self : Node) return Boolean;

   overriding function Is_Derived_Type_Element (Self : Node) return Boolean;

   overriding function Is_Derived_Record_Extension_Element
     (Self : Node) return Boolean;

   overriding function Is_Enumeration_Type_Element
     (Self : Node) return Boolean;

   overriding function Is_Signed_Integer_Type_Element
     (Self : Node) return Boolean;

   overriding function Is_Modular_Type_Element (Self : Node) return Boolean;

   overriding function Is_Root_Type_Element (Self : Node) return Boolean;

   overriding function Is_Floating_Point_Type_Element
     (Self : Node) return Boolean;

   overriding function Is_Ordinary_Fixed_Point_Type_Element
     (Self : Node) return Boolean;

   overriding function Is_Decimal_Fixed_Point_Type_Element
     (Self : Node) return Boolean;

   overriding function Is_Unconstrained_Array_Type_Element
     (Self : Node) return Boolean;

   overriding function Is_Constrained_Array_Type_Element
     (Self : Node) return Boolean;

   overriding function Is_Record_Type_Element (Self : Node) return Boolean;

   overriding function Is_Interface_Type_Element (Self : Node) return Boolean;

   overriding function Is_Object_Access_Type_Element
     (Self : Node) return Boolean;

   overriding function Is_Procedure_Access_Type_Element
     (Self : Node) return Boolean;

   overriding function Is_Function_Access_Type_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Private_Type_Definition_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Derived_Type_Definition_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Discrete_Type_Definition_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Signed_Integer_Type_Definition_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Modular_Type_Definition_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Floating_Point_Definition_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Ordinary_Fixed_Point_Definition_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Decimal_Fixed_Point_Definition_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Unconstrained_Array_Type_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Constrained_Array_Type_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Access_Type_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Object_Access_Type_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Procedure_Access_Type_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Function_Access_Type_Element
     (Self : Node) return Boolean;

   overriding function Is_Formal_Interface_Type_Element
     (Self : Node) return Boolean;

   overriding function Is_Access_Type_Element (Self : Node) return Boolean;

   overriding function Is_Range_Attribute_Reference_Element
     (Self : Node) return Boolean;

   overriding function Is_Simple_Expression_Range_Element
     (Self : Node) return Boolean;

   overriding function Is_Digits_Constraint_Element
     (Self : Node) return Boolean;

   overriding function Is_Delta_Constraint_Element
     (Self : Node) return Boolean;

   overriding function Is_Index_Constraint_Element
     (Self : Node) return Boolean;

   overriding function Is_Discriminant_Constraint_Element
     (Self : Node) return Boolean;

   overriding function Is_Attribute_Definition_Clause_Element
     (Self : Node) return Boolean;

   overriding function Is_Enumeration_Representation_Clause_Element
     (Self : Node) return Boolean;

   overriding function Is_Record_Representation_Clause_Element
     (Self : Node) return Boolean;

   overriding function Is_At_Clause_Element (Self : Node) return Boolean;

   overriding function Is_Exception_Handler_Element
     (Self : Node) return Boolean;

end Program.Nodes;
