package Asis.Extensions.Flat_Kinds is
   pragma Preelaborate;

   type Element_Flat_Kind is
     (Not_An_Element,
      --      A_Pragma,
      An_All_Calls_Remote_Pragma,
      An_Assert_Pragma,
      An_Assertion_Policy_Pragma,
      An_Asynchronous_Pragma,
      An_Atomic_Pragma,
      An_Atomic_Components_Pragma,
      An_Attach_Handler_Pragma,
      A_Controlled_Pragma,
      A_Convention_Pragma,
      A_Detect_Blocking_Pragma,
      A_Discard_Names_Pragma,
      An_Elaborate_Pragma,
      An_Elaborate_All_Pragma,
      An_Elaborate_Body_Pragma,
      An_Export_Pragma,
      An_Import_Pragma,
      An_Inline_Pragma,
      An_Inspection_Point_Pragma,
      An_Interrupt_Handler_Pragma,
      An_Interrupt_Priority_Pragma,
      A_Linker_Options_Pragma,
      A_List_Pragma,
      A_Locking_Policy_Pragma,
      A_No_Return_Pragma,
      A_Normalize_Scalars_Pragma,
      An_Optimize_Pragma,
      A_Pack_Pragma,
      A_Page_Pragma,
      A_Partition_Elaboration_Policy_Pragma,
      A_Preelaborable_Initialization_Pragma,
      A_Preelaborate_Pragma,
      A_Priority_Pragma,
      A_Priority_Specific_Dispatching_Pragma,
      A_Profile_Pragma,
      A_Pure_Pragma,
      A_Queuing_Policy_Pragma,
      A_Relative_Deadline_Pragma,
      A_Remote_Call_Interface_Pragma,
      A_Remote_Types_Pragma,
      A_Restrictions_Pragma,
      A_Reviewable_Pragma,
      A_Shared_Passive_Pragma,
      A_Storage_Size_Pragma,
      A_Suppress_Pragma,
      A_Task_Dispatching_Policy_Pragma,
      An_Unchecked_Union_Pragma,
      An_Unsuppress_Pragma,
      A_Volatile_Pragma,
      A_Volatile_Components_Pragma,
      An_Implementation_Defined_Pragma,
      An_Unknown_Pragma,
      --      A_Defining_Name,
      A_Defining_Identifier,
      A_Defining_Character_Literal,
      A_Defining_Enumeration_Literal,
      --  A_Defining_Operator_Symbol,
      A_Defining_And_Operator,
      A_Defining_Or_Operator,
      A_Defining_Xor_Operator,
      A_Defining_Equal_Operator,
      A_Defining_Not_Equal_Operator,
      A_Defining_Less_Than_Operator,
      A_Defining_Less_Than_Or_Equal_Operator,
      A_Defining_Greater_Than_Operator,
      A_Defining_Greater_Than_Or_Equal_Operator,
      A_Defining_Plus_Operator,
      A_Defining_Minus_Operator,
      A_Defining_Concatenate_Operator,
      A_Defining_Unary_Plus_Operator,
      A_Defining_Unary_Minus_Operator,
      A_Defining_Multiply_Operator,
      A_Defining_Divide_Operator,
      A_Defining_Mod_Operator,
      A_Defining_Rem_Operator,
      A_Defining_Exponentiate_Operator,
      A_Defining_Abs_Operator,
      A_Defining_Not_Operator,
      A_Defining_Expanded_Name,
      --      A_Declaration,
      An_Ordinary_Type_Declaration,
      A_Task_Type_Declaration,
      A_Protected_Type_Declaration,
      An_Incomplete_Type_Declaration,
      A_Private_Type_Declaration,
      A_Private_Extension_Declaration,
      A_Subtype_Declaration,
      A_Variable_Declaration,
      A_Constant_Declaration,
      A_Deferred_Constant_Declaration,
      A_Single_Task_Declaration,
      A_Single_Protected_Declaration,
      An_Integer_Number_Declaration,
      A_Real_Number_Declaration,
      An_Enumeration_Literal_Specification,
      A_Discriminant_Specification,
      A_Component_Declaration,
      A_Return_Object_Specification,
      A_Loop_Parameter_Specification,
      A_Procedure_Declaration,
      A_Function_Declaration,
      A_Parameter_Specification,
      A_Procedure_Body_Declaration,
      A_Function_Body_Declaration,
      A_Package_Declaration,
      A_Package_Body_Declaration,
      An_Object_Renaming_Declaration,
      An_Exception_Renaming_Declaration,
      A_Package_Renaming_Declaration,
      A_Procedure_Renaming_Declaration,
      A_Function_Renaming_Declaration,
      A_Generic_Package_Renaming_Declaration,
      A_Generic_Procedure_Renaming_Declaration,
      A_Generic_Function_Renaming_Declaration,
      A_Task_Body_Declaration,
      A_Protected_Body_Declaration,
      An_Entry_Declaration,
      An_Entry_Body_Declaration,
      An_Entry_Index_Specification,
      A_Procedure_Body_Stub,
      A_Function_Body_Stub,
      A_Package_Body_Stub,
      A_Task_Body_Stub,
      A_Protected_Body_Stub,
      An_Exception_Declaration,
      A_Choice_Parameter_Specification,
      A_Generic_Procedure_Declaration,
      A_Generic_Function_Declaration,
      A_Generic_Package_Declaration,
      A_Package_Instantiation,
      A_Procedure_Instantiation,
      A_Function_Instantiation,
      A_Formal_Object_Declaration,
      A_Formal_Type_Declaration,
      A_Formal_Procedure_Declaration,
      A_Formal_Function_Declaration,
      A_Formal_Package_Declaration,
      A_Formal_Package_Declaration_With_Box,
      --  A_Definition,
      --    A_Type_Definition,
      A_Derived_Type_Definition,
      A_Derived_Record_Extension_Definition,
      An_Enumeration_Type_Definition,
      A_Signed_Integer_Type_Definition,
      A_Modular_Type_Definition,
      --      A_Root_Type_Definition,
      A_Root_Integer_Definition,
      A_Root_Real_Definition,
      A_Universal_Integer_Definition,
      A_Universal_Real_Definition,
      A_Universal_Fixed_Definition,
      A_Universal_Access_Definition,
      A_Floating_Point_Definition,
      An_Ordinary_Fixed_Point_Definition,
      A_Decimal_Fixed_Point_Definition,
      An_Unconstrained_Array_Definition,
      A_Constrained_Array_Definition,
      A_Record_Type_Definition,
      A_Tagged_Record_Type_Definition,
      --      An_Interface_Type_Definition,
      An_Ordinary_Interface,
      A_Limited_Interface,
      A_Task_Interface,
      A_Protected_Interface,
      A_Synchronized_Interface,
      --      An_Access_Type_Definition,
      A_Pool_Specific_Access_To_Variable,
      An_Access_To_Variable,
      An_Access_To_Constant,
      An_Access_To_Procedure,
      An_Access_To_Protected_Procedure,
      An_Access_To_Function,
      An_Access_To_Protected_Function,
      A_Subtype_Indication,
      --      A_Constraint,
      A_Range_Attribute_Reference,
      A_Simple_Expression_Range,
      A_Digits_Constraint,
      A_Delta_Constraint,
      An_Index_Constraint,
      A_Discriminant_Constraint,
      A_Component_Definition,
      --      A_Discrete_Subtype_Definition,
      A_Discrete_Subtype_Indication,
      A_Discrete_Range_Attribute_Reference,
      A_Discrete_Simple_Expression_Range,
      --      A_Discrete_Range,
      A_Discrete_Subtype_Indication_DR,
      A_Discrete_Range_Attribute_Reference_DR,
      A_Discrete_Simple_Expression_Range_DR,
      An_Unknown_Discriminant_Part,
      A_Known_Discriminant_Part,
      A_Record_Definition,
      A_Null_Record_Definition,
      A_Null_Component,
      A_Variant_Part,
      A_Variant,
      An_Others_Choice,
      --      An_Access_Definition,
      An_Anonymous_Access_To_Variable,
      An_Anonymous_Access_To_Constant,
      An_Anonymous_Access_To_Procedure,
      An_Anonymous_Access_To_Protected_Procedure,
      An_Anonymous_Access_To_Function,
      An_Anonymous_Access_To_Protected_Function,
      An_Incomplete_Type_Definition,
      A_Tagged_Incomplete_Type_Definition,
      A_Private_Type_Definition,
      A_Tagged_Private_Type_Definition,
      A_Private_Extension_Definition,
      A_Task_Definition,
      A_Protected_Definition,
      --  A_Formal_Type_Definition,
      A_Formal_Private_Type_Definition,
      A_Formal_Tagged_Private_Type_Definition,
      A_Formal_Derived_Type_Definition,
      A_Formal_Discrete_Type_Definition,
      A_Formal_Signed_Integer_Type_Definition,
      A_Formal_Modular_Type_Definition,
      A_Formal_Floating_Point_Definition,
      A_Formal_Ordinary_Fixed_Point_Definition,
      A_Formal_Decimal_Fixed_Point_Definition,
      A_Formal_Unconstrained_Array_Definition,
      A_Formal_Constrained_Array_Definition,
      --  A_Formal_Access_Type_Definition,
      A_Formal_Pool_Specific_Access_To_Variable,
      A_Formal_Access_To_Variable,
      A_Formal_Access_To_Constant,
      A_Formal_Access_To_Procedure,
      A_Formal_Access_To_Protected_Procedure,
      A_Formal_Access_To_Function,
      A_Formal_Access_To_Protected_Function,
      --  A_Formal_Interface_Type_Definition,
      A_Formal_Ordinary_Interface,
      A_Formal_Limited_Interface,
      A_Formal_Task_Interface,
      A_Formal_Protected_Interface,
      A_Formal_Synchronized_Interface,
      --  An_Expression,
      A_Box_Expression,
      An_Integer_Literal,
      A_Real_Literal,
      A_String_Literal,
      An_Identifier,
      --  An_Operator_Symbol,
      An_And_Operator,
      An_Or_Operator,
      An_Xor_Operator,
      An_Equal_Operator,
      A_Not_Equal_Operator,
      A_Less_Than_Operator,
      A_Less_Than_Or_Equal_Operator,
      A_Greater_Than_Operator,
      A_Greater_Than_Or_Equal_Operator,
      A_Plus_Operator,
      A_Minus_Operator,
      A_Concatenate_Operator,
      A_Unary_Plus_Operator,
      A_Unary_Minus_Operator,
      A_Multiply_Operator,
      A_Divide_Operator,
      A_Mod_Operator,
      A_Rem_Operator,
      An_Exponentiate_Operator,
      An_Abs_Operator,
      A_Not_Operator,
      A_Character_Literal,
      An_Enumeration_Literal,
      An_Explicit_Dereference,
      A_Function_Call,
      An_Indexed_Component,
      A_Slice,
      A_Selected_Component,
      --  An_Attribute_Reference,
      An_Access_Attribute,
      An_Address_Attribute,
      An_Adjacent_Attribute,
      An_Aft_Attribute,
      An_Alignment_Attribute,
      A_Base_Attribute,
      A_Bit_Order_Attribute,
      A_Body_Version_Attribute,
      A_Callable_Attribute,
      A_Caller_Attribute,
      A_Ceiling_Attribute,
      A_Class_Attribute,
      A_Component_Size_Attribute,
      A_Compose_Attribute,
      A_Constrained_Attribute,
      A_Copy_Sign_Attribute,
      A_Count_Attribute,
      A_Definite_Attribute,
      A_Delta_Attribute,
      A_Denorm_Attribute,
      A_Digits_Attribute,
      An_Exponent_Attribute,
      An_External_Tag_Attribute,
      A_First_Attribute,
      A_First_Bit_Attribute,
      A_Floor_Attribute,
      A_Fore_Attribute,
      A_Fraction_Attribute,
      An_Identity_Attribute,
      An_Image_Attribute,
      An_Input_Attribute,
      A_Last_Attribute,
      A_Last_Bit_Attribute,
      A_Leading_Part_Attribute,
      A_Length_Attribute,
      A_Machine_Attribute,
      A_Machine_Emax_Attribute,
      A_Machine_Emin_Attribute,
      A_Machine_Mantissa_Attribute,
      A_Machine_Overflows_Attribute,
      A_Machine_Radix_Attribute,
      A_Machine_Rounding_Attribute,
      A_Machine_Rounds_Attribute,
      A_Max_Attribute,
      A_Max_Size_In_Storage_Elements_Attribute,
      A_Min_Attribute,
      A_Mod_Attribute,
      A_Model_Attribute,
      A_Model_Emin_Attribute,
      A_Model_Epsilon_Attribute,
      A_Model_Mantissa_Attribute,
      A_Model_Small_Attribute,
      A_Modulus_Attribute,
      An_Output_Attribute,
      A_Partition_ID_Attribute,
      A_Pos_Attribute,
      A_Position_Attribute,
      A_Pred_Attribute,
      A_Priority_Attribute,
      A_Range_Attribute,
      A_Read_Attribute,
      A_Remainder_Attribute,
      A_Round_Attribute,
      A_Rounding_Attribute,
      A_Safe_First_Attribute,
      A_Safe_Last_Attribute,
      A_Scale_Attribute,
      A_Scaling_Attribute,
      A_Signed_Zeros_Attribute,
      A_Size_Attribute,
      A_Small_Attribute,
      A_Storage_Pool_Attribute,
      A_Storage_Size_Attribute,
      A_Stream_Size_Attribute,
      A_Succ_Attribute,
      A_Tag_Attribute,
      A_Terminated_Attribute,
      A_Truncation_Attribute,
      An_Unbiased_Rounding_Attribute,
      An_Unchecked_Access_Attribute,
      A_Val_Attribute,
      A_Valid_Attribute,
      A_Value_Attribute,
      A_Version_Attribute,
      A_Wide_Image_Attribute,
      A_Wide_Value_Attribute,
      A_Wide_Wide_Image_Attribute,
      A_Wide_Wide_Value_Attribute,
      A_Wide_Wide_Width_Attribute,
      A_Wide_Width_Attribute,
      A_Width_Attribute,
      A_Write_Attribute,
      An_Implementation_Defined_Attribute,
      An_Unknown_Attribute,
      A_Record_Aggregate,
      An_Extension_Aggregate,
      A_Positional_Array_Aggregate,
      A_Named_Array_Aggregate,
      An_And_Then_Short_Circuit,
      An_Or_Else_Short_Circuit,
      An_In_Range_Membership_Test,
      A_Not_In_Range_Membership_Test,
      An_In_Type_Membership_Test,
      A_Not_In_Type_Membership_Test,
      A_Null_Literal,
      A_Parenthesized_Expression,
      A_Type_Conversion,
      A_Qualified_Expression,
      An_Allocation_From_Subtype,
      An_Allocation_From_Qualified_Expression,
      --  An_Association,
      A_Pragma_Argument_Association,
      A_Discriminant_Association,
      A_Record_Component_Association,
      An_Array_Component_Association,
      A_Parameter_Association,
      A_Generic_Association,
      --  A_Statement,
      A_Null_Statement,
      An_Assignment_Statement,
      An_If_Statement,
      A_Case_Statement,
      A_Loop_Statement,
      A_While_Loop_Statement,
      A_For_Loop_Statement,
      A_Block_Statement,
      An_Exit_Statement,
      A_Goto_Statement,
      A_Procedure_Call_Statement,
      A_Simple_Return_Statement,
      An_Extended_Return_Statement,
      An_Accept_Statement,
      An_Entry_Call_Statement,
      A_Requeue_Statement,
      A_Requeue_Statement_With_Abort,
      A_Delay_Until_Statement,
      A_Delay_Relative_Statement,
      A_Terminate_Alternative_Statement,
      A_Selective_Accept_Statement,
      A_Timed_Entry_Call_Statement,
      A_Conditional_Entry_Call_Statement,
      An_Asynchronous_Select_Statement,
      An_Abort_Statement,
      A_Raise_Statement,
      A_Code_Statement,
      --  A_Path,
      An_If_Path,
      An_Elsif_Path,
      An_Else_Path,
      A_Case_Path,
      A_Select_Path,
      An_Or_Path,
      A_Then_Abort_Path,
      --  A_Clause,
      A_Use_Package_Clause,
      A_Use_Type_Clause,
      A_With_Clause,
      --  A_Representation_Clause,
      An_Attribute_Definition_Clause,
      An_Enumeration_Representation_Clause,
      A_Record_Representation_Clause,
      An_At_Clause,
      A_Component_Clause,
      An_Exception_Handler);

   subtype A_Pragma is Element_Flat_Kind range
     An_All_Calls_Remote_Pragma .. An_Unknown_Pragma;

   subtype A_Defining_Name is Element_Flat_Kind range
     A_Defining_Identifier .. A_Defining_Expanded_Name;

   subtype A_Defining_Operator_Symbol is Element_Flat_Kind range
     A_Defining_And_Operator .. A_Defining_Not_Operator;

   subtype A_Declaration is Element_Flat_Kind range
     An_Ordinary_Type_Declaration .. A_Formal_Package_Declaration_With_Box;

   subtype A_Definition is Element_Flat_Kind range
     A_Derived_Type_Definition .. A_Formal_Synchronized_Interface;

   subtype A_Type_Definition is Element_Flat_Kind range
     A_Derived_Type_Definition .. An_Access_To_Protected_Function;

   subtype A_Constraint is Element_Flat_Kind range
     A_Range_Attribute_Reference .. A_Discriminant_Constraint;

   subtype A_Discrete_Subtype_Definition is Element_Flat_Kind range
     A_Discrete_Subtype_Indication .. A_Discrete_Simple_Expression_Range;

   subtype A_Discrete_Range is Element_Flat_Kind range
     A_Discrete_Subtype_Indication_DR .. A_Discrete_Simple_Expression_Range_DR;

   subtype An_Access_Definition is Element_Flat_Kind range
     An_Anonymous_Access_To_Variable
       .. An_Anonymous_Access_To_Protected_Function;

   subtype A_Formal_Type_Definition is Element_Flat_Kind range
     A_Formal_Private_Type_Definition .. A_Formal_Access_To_Protected_Function;

   subtype An_Expression is Element_Flat_Kind range
     A_Box_Expression .. An_Allocation_From_Qualified_Expression;

   subtype An_Association is Element_Flat_Kind range
     A_Pragma_Argument_Association .. A_Generic_Association;

   subtype A_Statement is Element_Flat_Kind range
     A_Null_Statement .. A_Code_Statement;

   subtype A_Path is Element_Flat_Kind range
     An_If_Path .. A_Then_Abort_Path;

   subtype A_Clause is Element_Flat_Kind range
     A_Use_Package_Clause .. A_With_Clause;

   subtype An_Operator_Symbol is Element_Flat_Kind range
     An_And_Operator .. A_Not_Operator;

   subtype An_Attribute_Reference is Element_Flat_Kind range
     An_Access_Attribute .. An_Unknown_Attribute;

   subtype A_Representation_Clause is Element_Flat_Kind range
     An_Attribute_Definition_Clause .. An_At_Clause;

   subtype A_Root_Type_Definition is Element_Flat_Kind range
      A_Root_Integer_Definition .. A_Universal_Access_Definition;

   subtype An_Interface_Type_Definition is Element_Flat_Kind range
     An_Ordinary_Interface .. A_Synchronized_Interface;

   subtype An_Access_Type_Definition is Element_Flat_Kind range
      A_Pool_Specific_Access_To_Variable .. An_Access_To_Protected_Function;

   function Flat_Kind (Element : Asis.Element) return Element_Flat_Kind;

end Asis.Extensions.Flat_Kinds;
