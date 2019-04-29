--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Program.Elements is

   pragma Pure (Program.Elements);

   type Element is limited interface;

   type Element_Access is access all Element'Class with Storage_Size => 0;

   not overriding function Is_Pragma
    (Self : Element)
      return Boolean is abstract;

   not overriding function Is_Defining_Name
    (Self : Element)
      return Boolean is abstract;

   not overriding function Is_Defining_Identifier
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Defining_Identifier'Result then Self.Is_Defining_Name);

   not overriding function Is_Defining_Character_Literal
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Defining_Character_Literal'Result then Self.Is_Defining_Name);

   not overriding function Is_Defining_Operator_Symbol
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Defining_Operator_Symbol'Result then Self.Is_Defining_Name);

   not overriding function Is_Defining_Expanded_Name
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Defining_Expanded_Name'Result then Self.Is_Defining_Name);

   not overriding function Is_Declaration
    (Self : Element)
      return Boolean is abstract;

   not overriding function Is_Type_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Type_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Task_Type_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Task_Type_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Protected_Type_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Protected_Type_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Subtype_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Subtype_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Object_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Object_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Single_Task_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Single_Task_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Single_Protected_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Single_Protected_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Number_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Number_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Enumeration_Literal_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Enumeration_Literal_Specification'Result
          then Self.Is_Declaration);

   not overriding function Is_Discriminant_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Discriminant_Specification'Result then Self.Is_Declaration);

   not overriding function Is_Component_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Component_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Loop_Parameter_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Loop_Parameter_Specification'Result then Self.Is_Declaration);

   not overriding function Is_Generalized_Iterator_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Generalized_Iterator_Specification'Result
          then Self.Is_Declaration);

   not overriding function Is_Element_Iterator_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Element_Iterator_Specification'Result then Self.Is_Declaration);

   not overriding function Is_Procedure_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Procedure_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Function_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Function_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Parameter_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Parameter_Specification'Result then Self.Is_Declaration);

   not overriding function Is_Procedure_Body_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Procedure_Body_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Function_Body_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Function_Body_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Return_Object_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Return_Object_Specification'Result then Self.Is_Declaration);

   not overriding function Is_Package_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Package_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Package_Body_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Package_Body_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Object_Renaming_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Object_Renaming_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Exception_Renaming_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Exception_Renaming_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Procedure_Renaming_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Procedure_Renaming_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Function_Renaming_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Function_Renaming_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Package_Renaming_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Package_Renaming_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Generic_Package_Renaming_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Generic_Package_Renaming_Declaration'Result
          then Self.Is_Declaration);

   not overriding function Is_Generic_Procedure_Renaming_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Generic_Procedure_Renaming_Declaration'Result
          then Self.Is_Declaration);

   not overriding function Is_Generic_Function_Renaming_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Generic_Function_Renaming_Declaration'Result
          then Self.Is_Declaration);

   not overriding function Is_Task_Body_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Task_Body_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Protected_Body_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Protected_Body_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Entry_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Entry_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Entry_Body_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Entry_Body_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Entry_Index_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Entry_Index_Specification'Result then Self.Is_Declaration);

   not overriding function Is_Procedure_Body_Stub
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Procedure_Body_Stub'Result then Self.Is_Declaration);

   not overriding function Is_Function_Body_Stub
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Function_Body_Stub'Result then Self.Is_Declaration);

   not overriding function Is_Package_Body_Stub
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Package_Body_Stub'Result then Self.Is_Declaration);

   not overriding function Is_Task_Body_Stub
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Task_Body_Stub'Result then Self.Is_Declaration);

   not overriding function Is_Protected_Body_Stub
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Protected_Body_Stub'Result then Self.Is_Declaration);

   not overriding function Is_Exception_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Exception_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Choice_Parameter_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Choice_Parameter_Specification'Result then Self.Is_Declaration);

   not overriding function Is_Generic_Package_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Generic_Package_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Generic_Procedure_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Generic_Procedure_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Generic_Function_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Generic_Function_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Package_Instantiation
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Package_Instantiation'Result then Self.Is_Declaration);

   not overriding function Is_Procedure_Instantiation
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Procedure_Instantiation'Result then Self.Is_Declaration);

   not overriding function Is_Function_Instantiation
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Function_Instantiation'Result then Self.Is_Declaration);

   not overriding function Is_Formal_Object_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Object_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Formal_Type_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Type_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Formal_Procedure_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Procedure_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Formal_Function_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Function_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Formal_Package_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Package_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Definition
    (Self : Element)
      return Boolean is abstract;

   not overriding function Is_Type_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Type_Definition'Result then Self.Is_Definition);

   not overriding function Is_Subtype_Indication
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Subtype_Indication'Result
          then Self.Is_Definition or Self.Is_Discrete_Subtype_Definition
            or Self.Is_Discrete_Range);

   not overriding function Is_Constraint
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Constraint'Result then Self.Is_Definition);

   not overriding function Is_Component_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Component_Definition'Result then Self.Is_Definition);

   not overriding function Is_Discrete_Subtype_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Discrete_Subtype_Definition'Result then Self.Is_Definition);

   not overriding function Is_Discrete_Range
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Discrete_Range'Result then Self.Is_Definition);

   not overriding function Is_Unknown_Discriminant_Part
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Unknown_Discriminant_Part'Result then Self.Is_Definition);

   not overriding function Is_Known_Discriminant_Part
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Known_Discriminant_Part'Result then Self.Is_Definition);

   not overriding function Is_Record_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Record_Definition'Result then Self.Is_Definition);

   not overriding function Is_Null_Component
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Null_Component'Result then Self.Is_Definition);

   not overriding function Is_Variant_Part
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Variant_Part'Result then Self.Is_Definition);

   not overriding function Is_Variant
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Variant'Result then Self.Is_Definition);

   not overriding function Is_Others_Choice
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Others_Choice'Result then Self.Is_Definition);

   not overriding function Is_Anonymous_Access_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Anonymous_Access_Definition'Result then Self.Is_Definition);

   not overriding function Is_Private_Type_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Private_Type_Definition'Result then Self.Is_Definition);

   not overriding function Is_Private_Extension_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Private_Extension_Definition'Result then Self.Is_Definition);

   not overriding function Is_Incomplete_Type_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Incomplete_Type_Definition'Result then Self.Is_Definition);

   not overriding function Is_Task_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Task_Definition'Result then Self.Is_Definition);

   not overriding function Is_Protected_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Protected_Definition'Result then Self.Is_Definition);

   not overriding function Is_Formal_Type_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Type_Definition'Result then Self.Is_Definition);

   not overriding function Is_Aspect_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Aspect_Specification'Result then Self.Is_Definition);

   not overriding function Is_Expression
    (Self : Element)
      return Boolean is abstract;

   not overriding function Is_Box_Expression
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Box_Expression'Result then Self.Is_Expression);

   not overriding function Is_Numeric_Literal
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Numeric_Literal'Result then Self.Is_Expression);

   not overriding function Is_String_Literal
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_String_Literal'Result then Self.Is_Expression);

   not overriding function Is_Identifier
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Identifier'Result then Self.Is_Expression);

   not overriding function Is_Operator_Symbol
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Operator_Symbol'Result then Self.Is_Expression);

   not overriding function Is_Character_Literal
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Character_Literal'Result then Self.Is_Expression);

   not overriding function Is_Explicit_Dereference
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Explicit_Dereference'Result then Self.Is_Expression);

   not overriding function Is_Function_Call
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Function_Call'Result then Self.Is_Expression);

   not overriding function Is_Indexed_Component
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Indexed_Component'Result then Self.Is_Expression);

   not overriding function Is_Slice (Self : Element) return Boolean is abstract
     with Post'Class => (if Is_Slice'Result then Self.Is_Expression);

   not overriding function Is_Selected_Component
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Selected_Component'Result then Self.Is_Expression);

   not overriding function Is_Attribute_Reference
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Attribute_Reference'Result then Self.Is_Expression);

   not overriding function Is_Record_Aggregate
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Record_Aggregate'Result then Self.Is_Expression);

   not overriding function Is_Extension_Aggregate
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Extension_Aggregate'Result then Self.Is_Expression);

   not overriding function Is_Array_Aggregate
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Array_Aggregate'Result then Self.Is_Expression);

   not overriding function Is_Short_Circuit_Operation
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Short_Circuit_Operation'Result then Self.Is_Expression);

   not overriding function Is_Membership_Test
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Membership_Test'Result then Self.Is_Expression);

   not overriding function Is_Null_Literal
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Null_Literal'Result then Self.Is_Expression);

   not overriding function Is_Parenthesized_Expression
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Parenthesized_Expression'Result then Self.Is_Expression);

   not overriding function Is_Raise_Expression
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Raise_Expression'Result then Self.Is_Expression);

   not overriding function Is_Type_Conversion
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Type_Conversion'Result then Self.Is_Expression);

   not overriding function Is_Qualified_Expression
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Qualified_Expression'Result then Self.Is_Expression);

   not overriding function Is_Allocator
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Allocator'Result then Self.Is_Expression);

   not overriding function Is_Case_Expression
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Case_Expression'Result then Self.Is_Expression);

   not overriding function Is_If_Expression
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_If_Expression'Result then Self.Is_Expression);

   not overriding function Is_Quantified_Expression
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Quantified_Expression'Result then Self.Is_Expression);

   not overriding function Is_Association
    (Self : Element)
      return Boolean is abstract;

   not overriding function Is_Discriminant_Association
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Discriminant_Association'Result then Self.Is_Association);

   not overriding function Is_Record_Component_Association
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Record_Component_Association'Result then Self.Is_Association);

   not overriding function Is_Array_Component_Association
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Array_Component_Association'Result then Self.Is_Association);

   not overriding function Is_Parameter_Association
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Parameter_Association'Result then Self.Is_Association);

   not overriding function Is_Statement
    (Self : Element)
      return Boolean is abstract;

   not overriding function Is_Null_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Null_Statement'Result then Self.Is_Statement);

   not overriding function Is_Assignment_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Assignment_Statement'Result then Self.Is_Statement);

   not overriding function Is_If_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_If_Statement'Result then Self.Is_Statement);

   not overriding function Is_Case_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Case_Statement'Result then Self.Is_Statement);

   not overriding function Is_Loop_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Loop_Statement'Result then Self.Is_Statement);

   not overriding function Is_While_Loop_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_While_Loop_Statement'Result then Self.Is_Statement);

   not overriding function Is_For_Loop_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_For_Loop_Statement'Result then Self.Is_Statement);

   not overriding function Is_Block_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Block_Statement'Result then Self.Is_Statement);

   not overriding function Is_Exit_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Exit_Statement'Result then Self.Is_Statement);

   not overriding function Is_Goto_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Goto_Statement'Result then Self.Is_Statement);

   not overriding function Is_Call_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Call_Statement'Result then Self.Is_Statement);

   not overriding function Is_Simple_Return_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Simple_Return_Statement'Result then Self.Is_Statement);

   not overriding function Is_Extended_Return_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Extended_Return_Statement'Result then Self.Is_Statement);

   not overriding function Is_Accept_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Accept_Statement'Result then Self.Is_Statement);

   not overriding function Is_Requeue_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Requeue_Statement'Result then Self.Is_Statement);

   not overriding function Is_Delay_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Delay_Statement'Result then Self.Is_Statement);

   not overriding function Is_Terminate_Alternative_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Terminate_Alternative_Statement'Result then Self.Is_Statement);

   not overriding function Is_Select_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Select_Statement'Result then Self.Is_Statement);

   not overriding function Is_Abort_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Abort_Statement'Result then Self.Is_Statement);

   not overriding function Is_Raise_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Raise_Statement'Result then Self.Is_Statement);

   not overriding function Is_Code_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Code_Statement'Result then Self.Is_Statement);

   not overriding function Is_Path (Self : Element) return Boolean is abstract;

   not overriding function Is_Elsif_Path
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Elsif_Path'Result then Self.Is_Path);

   not overriding function Is_Case_Path
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Case_Path'Result then Self.Is_Path);

   not overriding function Is_Select_Path
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Select_Path'Result then Self.Is_Path);

   not overriding function Is_Then_Abort_Path
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Then_Abort_Path'Result then Self.Is_Path);

   not overriding function Is_Case_Expression_Path
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Case_Expression_Path'Result then Self.Is_Path);

   not overriding function Is_Elsif_Expression_Path
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Elsif_Expression_Path'Result then Self.Is_Path);

   not overriding function Is_Clause
    (Self : Element)
      return Boolean is abstract;

   not overriding function Is_Use_Clause
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Use_Clause'Result then Self.Is_Clause);

   not overriding function Is_With_Clause
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_With_Clause'Result then Self.Is_Clause);

   not overriding function Is_Representation_Clause
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Representation_Clause'Result then Self.Is_Clause);

   not overriding function Is_Component_Clause
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Component_Clause'Result then Self.Is_Clause);

   not overriding function Is_Derived_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Derived_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Derived_Record_Extension
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Derived_Record_Extension'Result then Self.Is_Type_Definition);

   not overriding function Is_Enumeration_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Enumeration_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Signed_Integer_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Signed_Integer_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Modular_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Modular_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Root_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Root_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Floating_Point_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Floating_Point_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Ordinary_Fixed_Point_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Ordinary_Fixed_Point_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Decimal_Fixed_Point_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Decimal_Fixed_Point_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Unconstrained_Array_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Unconstrained_Array_Type'Result
          then Self.Is_Type_Definition or Self.Is_Formal_Type_Definition);

   not overriding function Is_Constrained_Array_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Constrained_Array_Type'Result
          then Self.Is_Type_Definition or Self.Is_Formal_Type_Definition);

   not overriding function Is_Record_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Record_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Interface_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Interface_Type'Result
          then Self.Is_Type_Definition or Self.Is_Formal_Type_Definition);

   not overriding function Is_Object_Access_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Object_Access_Type'Result
          then Self.Is_Access_Type or Self.Is_Formal_Access_Type
            or Self.Is_Anonymous_Access_Definition);

   not overriding function Is_Procedure_Access_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Procedure_Access_Type'Result
          then Self.Is_Access_Type or Self.Is_Formal_Access_Type
            or Self.Is_Anonymous_Access_Definition);

   not overriding function Is_Function_Access_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Function_Access_Type'Result
          then Self.Is_Access_Type or Self.Is_Formal_Access_Type
            or Self.Is_Anonymous_Access_Definition);

   not overriding function Is_Formal_Private_Type_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Private_Type_Definition'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Derived_Type_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Derived_Type_Definition'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Discrete_Type_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Discrete_Type_Definition'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Signed_Integer_Type_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Signed_Integer_Type_Definition'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Modular_Type_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Modular_Type_Definition'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Floating_Point_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Floating_Point_Definition'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Ordinary_Fixed_Point_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Ordinary_Fixed_Point_Definition'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Decimal_Fixed_Point_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Decimal_Fixed_Point_Definition'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Access_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Access_Type'Result then Self.Is_Formal_Type_Definition);

   not overriding function Is_Access_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Access_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Range_Attribute_Reference
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Range_Attribute_Reference'Result
          then Self.Is_Constraint or Self.Is_Discrete_Subtype_Definition
            or Self.Is_Discrete_Range);

   not overriding function Is_Simple_Expression_Range
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Simple_Expression_Range'Result
          then Self.Is_Constraint or Self.Is_Discrete_Subtype_Definition
            or Self.Is_Discrete_Range);

   not overriding function Is_Digits_Constraint
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Digits_Constraint'Result then Self.Is_Constraint);

   not overriding function Is_Delta_Constraint
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Delta_Constraint'Result then Self.Is_Constraint);

   not overriding function Is_Index_Constraint
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Index_Constraint'Result then Self.Is_Constraint);

   not overriding function Is_Discriminant_Constraint
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Discriminant_Constraint'Result then Self.Is_Constraint);

   not overriding function Is_Attribute_Definition_Clause
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Attribute_Definition_Clause'Result
          then Self.Is_Representation_Clause);

   not overriding function Is_Enumeration_Representation_Clause
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Enumeration_Representation_Clause'Result
          then Self.Is_Representation_Clause);

   not overriding function Is_Record_Representation_Clause
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Record_Representation_Clause'Result
          then Self.Is_Representation_Clause);

   not overriding function Is_At_Clause
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_At_Clause'Result then Self.Is_Representation_Clause);

end Program.Elements;
