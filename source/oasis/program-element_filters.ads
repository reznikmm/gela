--  SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements;

package Program.Element_Filters is
   pragma Pure;

   function Is_Pragma
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Pragma);

   function Is_Defining_Name
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Defining_Name);

   function Is_Defining_Identifier
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Defining_Identifier);

   function Is_Defining_Character_Literal
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Defining_Character_Literal);

   function Is_Defining_Operator_Symbol
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Defining_Operator_Symbol);

   function Is_Defining_Expanded_Name
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Defining_Expanded_Name);

   function Is_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Declaration);

   function Is_Type_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Type_Declaration);

   function Is_Task_Type_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Task_Type_Declaration);

   function Is_Protected_Type_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Protected_Type_Declaration);

   function Is_Subtype_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Subtype_Declaration);

   function Is_Object_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Object_Declaration);

   function Is_Single_Task_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Single_Task_Declaration);

   function Is_Single_Protected_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Single_Protected_Declaration);

   function Is_Number_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Number_Declaration);

   function Is_Enumeration_Literal_Specification
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Enumeration_Literal_Specification);

   function Is_Discriminant_Specification
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Discriminant_Specification);

   function Is_Component_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Component_Declaration);

   function Is_Loop_Parameter_Specification
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Loop_Parameter_Specification);

   function Is_Generalized_Iterator_Specification
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Generalized_Iterator_Specification);

   function Is_Element_Iterator_Specification
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Element_Iterator_Specification);

   function Is_Procedure_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Procedure_Declaration);

   function Is_Function_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Function_Declaration);

   function Is_Parameter_Specification
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Parameter_Specification);

   function Is_Procedure_Body_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Procedure_Body_Declaration);

   function Is_Function_Body_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Function_Body_Declaration);

   function Is_Return_Object_Specification
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Return_Object_Specification);

   function Is_Package_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Package_Declaration);

   function Is_Package_Body_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Package_Body_Declaration);

   function Is_Object_Renaming_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Object_Renaming_Declaration);

   function Is_Exception_Renaming_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Exception_Renaming_Declaration);

   function Is_Procedure_Renaming_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Procedure_Renaming_Declaration);

   function Is_Function_Renaming_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Function_Renaming_Declaration);

   function Is_Package_Renaming_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Package_Renaming_Declaration);

   function Is_Generic_Package_Renaming_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Generic_Package_Renaming_Declaration);

   function Is_Generic_Procedure_Renaming_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Generic_Procedure_Renaming_Declaration);

   function Is_Generic_Function_Renaming_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Generic_Function_Renaming_Declaration);

   function Is_Task_Body_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Task_Body_Declaration);

   function Is_Protected_Body_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Protected_Body_Declaration);

   function Is_Entry_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Entry_Declaration);

   function Is_Entry_Body_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Entry_Body_Declaration);

   function Is_Entry_Index_Specification
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Entry_Index_Specification);

   function Is_Procedure_Body_Stub
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Procedure_Body_Stub);

   function Is_Function_Body_Stub
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Function_Body_Stub);

   function Is_Package_Body_Stub
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Package_Body_Stub);

   function Is_Task_Body_Stub
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Task_Body_Stub);

   function Is_Protected_Body_Stub
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Protected_Body_Stub);

   function Is_Exception_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Exception_Declaration);

   function Is_Choice_Parameter_Specification
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Choice_Parameter_Specification);

   function Is_Generic_Package_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Generic_Package_Declaration);

   function Is_Generic_Procedure_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Generic_Procedure_Declaration);

   function Is_Generic_Function_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Generic_Function_Declaration);

   function Is_Package_Instantiation
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Package_Instantiation);

   function Is_Procedure_Instantiation
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Procedure_Instantiation);

   function Is_Function_Instantiation
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Function_Instantiation);

   function Is_Formal_Object_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Object_Declaration);

   function Is_Formal_Type_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Type_Declaration);

   function Is_Formal_Procedure_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Procedure_Declaration);

   function Is_Formal_Function_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Function_Declaration);

   function Is_Formal_Package_Declaration
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Package_Declaration);

   function Is_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Definition);

   function Is_Type_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Type_Definition);

   function Is_Subtype_Indication
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Subtype_Indication);

   function Is_Constraint
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Constraint);

   function Is_Component_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Component_Definition);

   function Is_Discrete_Range
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Discrete_Range);

   function Is_Discrete_Subtype_Indication
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Discrete_Subtype_Indication);

   function Is_Discrete_Range_Attribute_Reference
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Discrete_Range_Attribute_Reference);

   function Is_Discrete_Simple_Expression_Range
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Discrete_Simple_Expression_Range);

   function Is_Unknown_Discriminant_Part
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Unknown_Discriminant_Part);

   function Is_Known_Discriminant_Part
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Known_Discriminant_Part);

   function Is_Record_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Record_Definition);

   function Is_Null_Component
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Null_Component);

   function Is_Variant_Part
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Variant_Part);

   function Is_Variant
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Variant);

   function Is_Others_Choice
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Others_Choice);

   function Is_Anonymous_Access_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Anonymous_Access_Definition);

   function Is_Anonymous_Access_To_Object
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Anonymous_Access_To_Object);

   function Is_Anonymous_Access_To_Procedure
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Anonymous_Access_To_Procedure);

   function Is_Anonymous_Access_To_Function
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Anonymous_Access_To_Function);

   function Is_Private_Type_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Private_Type_Definition);

   function Is_Private_Extension_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Private_Extension_Definition);

   function Is_Incomplete_Type_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Incomplete_Type_Definition);

   function Is_Task_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Task_Definition);

   function Is_Protected_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Protected_Definition);

   function Is_Formal_Type_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Type_Definition);

   function Is_Aspect_Specification
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Aspect_Specification);

   function Is_Real_Range_Specification
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Real_Range_Specification);

   function Is_Expression
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Expression);

   function Is_Numeric_Literal
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Numeric_Literal);

   function Is_String_Literal
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_String_Literal);

   function Is_Identifier
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Identifier);

   function Is_Operator_Symbol
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Operator_Symbol);

   function Is_Character_Literal
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Character_Literal);

   function Is_Explicit_Dereference
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Explicit_Dereference);

   function Is_Infix_Operator
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Infix_Operator);

   function Is_Function_Call
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Function_Call);

   function Is_Indexed_Component
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Indexed_Component);

   function Is_Slice
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Slice);

   function Is_Selected_Component
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Selected_Component);

   function Is_Attribute_Reference
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Attribute_Reference);

   function Is_Record_Aggregate
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Record_Aggregate);

   function Is_Extension_Aggregate
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Extension_Aggregate);

   function Is_Array_Aggregate
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Array_Aggregate);

   function Is_Short_Circuit_Operation
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Short_Circuit_Operation);

   function Is_Membership_Test
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Membership_Test);

   function Is_Null_Literal
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Null_Literal);

   function Is_Parenthesized_Expression
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Parenthesized_Expression);

   function Is_Raise_Expression
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Raise_Expression);

   function Is_Type_Conversion
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Type_Conversion);

   function Is_Qualified_Expression
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Qualified_Expression);

   function Is_Allocator
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Allocator);

   function Is_Case_Expression
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Case_Expression);

   function Is_If_Expression
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_If_Expression);

   function Is_Quantified_Expression
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Quantified_Expression);

   function Is_Association
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Association);

   function Is_Discriminant_Association
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Discriminant_Association);

   function Is_Record_Component_Association
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Record_Component_Association);

   function Is_Array_Component_Association
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Array_Component_Association);

   function Is_Parameter_Association
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Parameter_Association);

   function Is_Formal_Package_Association
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Package_Association);

   function Is_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Statement);

   function Is_Null_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Null_Statement);

   function Is_Assignment_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Assignment_Statement);

   function Is_If_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_If_Statement);

   function Is_Case_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Case_Statement);

   function Is_Loop_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Loop_Statement);

   function Is_While_Loop_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_While_Loop_Statement);

   function Is_For_Loop_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_For_Loop_Statement);

   function Is_Block_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Block_Statement);

   function Is_Exit_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Exit_Statement);

   function Is_Goto_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Goto_Statement);

   function Is_Call_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Call_Statement);

   function Is_Simple_Return_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Simple_Return_Statement);

   function Is_Extended_Return_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Extended_Return_Statement);

   function Is_Accept_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Accept_Statement);

   function Is_Requeue_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Requeue_Statement);

   function Is_Delay_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Delay_Statement);

   function Is_Terminate_Alternative_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Terminate_Alternative_Statement);

   function Is_Select_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Select_Statement);

   function Is_Abort_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Abort_Statement);

   function Is_Raise_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Raise_Statement);

   function Is_Code_Statement
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Code_Statement);

   function Is_Path
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Path);

   function Is_Elsif_Path
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Elsif_Path);

   function Is_Case_Path
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Case_Path);

   function Is_Select_Path
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Select_Path);

   function Is_Case_Expression_Path
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Case_Expression_Path);

   function Is_Elsif_Expression_Path
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Elsif_Expression_Path);

   function Is_Clause
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Clause);

   function Is_Use_Clause
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Use_Clause);

   function Is_With_Clause
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_With_Clause);

   function Is_Representation_Clause
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Representation_Clause);

   function Is_Component_Clause
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Component_Clause);

   function Is_Derived_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Derived_Type);

   function Is_Derived_Record_Extension
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Derived_Record_Extension);

   function Is_Enumeration_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Enumeration_Type);

   function Is_Signed_Integer_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Signed_Integer_Type);

   function Is_Modular_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Modular_Type);

   function Is_Root_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Root_Type);

   function Is_Floating_Point_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Floating_Point_Type);

   function Is_Ordinary_Fixed_Point_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Ordinary_Fixed_Point_Type);

   function Is_Decimal_Fixed_Point_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Decimal_Fixed_Point_Type);

   function Is_Unconstrained_Array_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Unconstrained_Array_Type);

   function Is_Constrained_Array_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Constrained_Array_Type);

   function Is_Record_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Record_Type);

   function Is_Interface_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Interface_Type);

   function Is_Object_Access_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Object_Access_Type);

   function Is_Procedure_Access_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Procedure_Access_Type);

   function Is_Function_Access_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Function_Access_Type);

   function Is_Formal_Private_Type_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Private_Type_Definition);

   function Is_Formal_Derived_Type_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Derived_Type_Definition);

   function Is_Formal_Discrete_Type_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Discrete_Type_Definition);

   function Is_Formal_Signed_Integer_Type_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Signed_Integer_Type_Definition);

   function Is_Formal_Modular_Type_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Modular_Type_Definition);

   function Is_Formal_Floating_Point_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Floating_Point_Definition);

   function Is_Formal_Ordinary_Fixed_Point_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Ordinary_Fixed_Point_Definition);

   function Is_Formal_Decimal_Fixed_Point_Definition
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Decimal_Fixed_Point_Definition);

   function Is_Formal_Unconstrained_Array_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Unconstrained_Array_Type);

   function Is_Formal_Constrained_Array_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Constrained_Array_Type);

   function Is_Formal_Access_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Access_Type);

   function Is_Formal_Object_Access_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Object_Access_Type);

   function Is_Formal_Procedure_Access_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Procedure_Access_Type);

   function Is_Formal_Function_Access_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Function_Access_Type);

   function Is_Formal_Interface_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Formal_Interface_Type);

   function Is_Access_Type
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Access_Type);

   function Is_Range_Attribute_Reference
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Range_Attribute_Reference);

   function Is_Simple_Expression_Range
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Simple_Expression_Range);

   function Is_Digits_Constraint
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Digits_Constraint);

   function Is_Delta_Constraint
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Delta_Constraint);

   function Is_Index_Constraint
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Index_Constraint);

   function Is_Discriminant_Constraint
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Discriminant_Constraint);

   function Is_Attribute_Definition_Clause
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Attribute_Definition_Clause);

   function Is_Enumeration_Representation_Clause
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Enumeration_Representation_Clause);

   function Is_Record_Representation_Clause
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Record_Representation_Clause);

   function Is_At_Clause
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_At_Clause);

   function Is_Exception_Handler
     (Self : not null Program.Elements.Element_Access) return Boolean is
       (Self.Is_Exception_Handler);

end Program.Element_Filters;
