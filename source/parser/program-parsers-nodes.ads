--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

private package Program.Parsers.Nodes is
   pragma Preelaborate;

   type Node is private;
   type Node_Array is array (Positive range <>) of Node;

   None     : constant Node;
   No_Token : constant Node;

   type Node_Factory is tagged limited private;

   function Compilation
     (Self : Node_Factory'Class; Units : Node; Compilation_Pragmas : Node)
      return Node;

   function Abort_Statement
     (Self : Node_Factory'Class; Abort_Token : Node; Aborted_Tasks : Node;
      Semicolon_Token : Node) return Node;

   function Accept_Statement
     (Self                     : Node_Factory'Class; Accept_Token : Node;
      Accept_Entry_Direct_Name : Node; Left_Parenthesis_Token : Node;
      Accept_Entry_Index       : Node; Right_Parenthesis_Token : Node;
      Lp_Token : Node; Accept_Parameters : Node; Rp_Token : Node;
      Do_Token : Node; Accept_Body_Statements : Node; Exception_Token : Node;
      Exception_Handlers : Node; End_Token : Node; Identifier_Token : Node;
      Semicolon_Token          : Node) return Node;

   function Access_To_Function_Definition
     (Self : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Access_Token      : Node; Protected_Token : Node; Function_Token : Node;
      Lp_Token          : Node; Access_To_Subprogram_Parameter_Profile : Node;
      Rp_Token          : Node; Return_Token : Node; Return_Not_Token : Node;
      Return_Null_Token : Node; Access_To_Function_Result_Subtype : Node)
      return Node;

   function Access_To_Object_Definition
     (Self         : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Access_Token : Node; Constant_Token : Node; Subtype_Indication : Node)
      return Node;

   function Access_To_Procedure_Definition
     (Self         : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Access_Token : Node; Protected_Token : Node; Procedure_Token : Node;
      Lp_Token     : Node; Access_To_Subprogram_Parameter_Profile : Node;
      Rp_Token     : Node) return Node;

   function Allocator
     (Self                    : Node_Factory'Class; New_Token : Node;
      Left_Parenthesis_Token  : Node; Subpool_Name : Node;
      Right_Parenthesis_Token : Node; Subtype_Or_Expression : Node)
      return Node;

   function Anonymous_Access_To_Function_Definition
     (Self : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Access_Token      : Node; Protected_Token : Node; Function_Token : Node;
      Lp_Token          : Node; Access_To_Subprogram_Parameter_Profile : Node;
      Rp_Token          : Node; Return_Token : Node; Return_Not_Token : Node;
      Return_Null_Token : Node; Access_To_Function_Result_Subtype : Node)
      return Node;

   function Anonymous_Access_To_Object_Definition
     (Self : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Access_Token                            : Node; Constant_Token : Node;
      Anonymous_Access_To_Object_Subtype_Mark : Node) return Node;

   function Anonymous_Access_To_Procedure_Definition
     (Self         : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Access_Token : Node; Protected_Token : Node; Procedure_Token : Node;
      Lp_Token     : Node; Access_To_Subprogram_Parameter_Profile : Node;
      Rp_Token     : Node) return Node;

   function Aspect_Specification
     (Self : Node_Factory'Class; Aspect_Mark : Node; Arrow_Token : Node;
      Aspect_Definition : Node) return Node;

   function Assignment_Statement
     (Self             : Node_Factory'Class; Assignment_Variable_Name : Node;
      Assignment_Token : Node; Assignment_Expression : Node;
      Semicolon_Token  : Node) return Node;

   function Association
     (Self        : Node_Factory'Class; Array_Component_Choices : Node;
      Arrow_Token : Node; Component_Expression : Node) return Node;

   function Association_List
     (Self                          : Node_Factory'Class; Left_Token : Node;
      Record_Component_Associations : Node; Right_Token : Node) return Node;

   function Asynchronous_Select
     (Self                         : Node_Factory'Class; Select_Token : Node;
      Asynchronous_Statement_Paths : Node; End_Token : Node; End_Select : Node;
      Semicolon_Token              : Node) return Node;

   function At_Clause
     (Self                             : Node_Factory'Class; For_Token : Node;
      Representation_Clause_Name : Node; Use_Token : Node; At_Token : Node;
      Representation_Clause_Expression : Node; Semicolon_Token : Node)
      return Node;

   function Attribute_Definition_Clause
     (Self                             : Node_Factory'Class; For_Token : Node;
      Representation_Clause_Name       : Node; Use_Token : Node;
      Representation_Clause_Expression : Node; Semicolon_Token : Node)
      return Node;

   function Attribute_Reference
     (Self : Node_Factory'Class; Prefix : Node; Apostrophe_Token : Node;
      Attribute_Designator_Identifier  : Node;
      Attribute_Designator_Expressions : Node) return Node;

   function Block_Statement
     (Self               : Node_Factory'Class; Statement_Identifier : Node;
      Colon_Token : Node; Declare_Token : Node; Block_Declarative_Items : Node;
      Begin_Token : Node; Block_Statements : Node; Exception_Token : Node;
      Exception_Handlers : Node; End_Token : Node; Identifier_Token : Node;
      Semicolon_Token    : Node) return Node;

   function Box (Self : Node_Factory'Class; Box_Token : Node) return Node;

   function Case_Expression
     (Self     : Node_Factory'Class; Case_Token : Node; Expression : Node;
      Is_Token : Node; Case_Expression_Paths : Node) return Node;

   function Case_Expression_Path
     (Self                          : Node_Factory'Class; When_Token : Node;
      Case_Path_Alternative_Choices : Node; Arrow_Token : Node;
      Dependent_Expression          : Node) return Node;

   function Case_Path
     (Self : Node_Factory'Class; When_Token : Node; Variant_Choices : Node;
      Arrow_Token : Node; Sequence_Of_Statements : Node) return Node;

   function Case_Statement
     (Self     : Node_Factory'Class; Case_Token : Node; Case_Expression : Node;
      Is_Token : Node; Case_Statement_Paths : Node; End_Token : Node;
      Endcase  : Node; Semicolon_Token : Node) return Node;

   function Character_Literal
     (Self : Node_Factory'Class; Character_Literal_Token : Node) return Node;

   function Choice_Parameter_Specification
     (Self : Node_Factory'Class; Names : Node) return Node;

   function Compilation_Unit_Body
     (Self             : Node_Factory'Class; Context_Clause_Elements : Node;
      Unit_Declaration : Node) return Node;

   function Compilation_Unit_Declaration
     (Self          : Node_Factory'Class; Context_Clause_Elements : Node;
      Private_Token : Node; Unit_Declaration : Node) return Node;

   function Component_Clause
     (Self : Node_Factory'Class; Representation_Clause_Name : Node;
      At_Token : Node; Component_Clause_Position : Node; Range_Token : Node;
      Component_Clause_Range : Node; Semicolon_Token : Node) return Node;

   function Component_Declaration
     (Self : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Object_Declaration_Subtype : Node; Assignment_Token : Node;
      Initialization_Expression  : Node; Aspect_Specifications : Node;
      Semicolon_Token            : Node) return Node;

   function Component_Definition
     (Self                         : Node_Factory'Class; Aliased_Token : Node;
      Component_Subtype_Indication : Node) return Node;

   function Composite_Constraint
     (Self        : Node_Factory'Class; Left_Token : Node; Associations : Node;
      Right_Token : Node) return Node;

   function Composite_Subtype_Indication
     (Self         : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Subtype_Mark : Node; Composite_Constraint : Node) return Node;

   function Constrained_Array_Definition
     (Self : Node_Factory'Class; Array_Token : Node; Left_Token : Node;
      Discrete_Subtype_Definitions : Node; Right_Token : Node; Of_Token : Node;
      Array_Component_Definition   : Node) return Node;

   function Decimal_Fixed_Point_Definition
     (Self : Node_Factory'Class; Delta_Token : Node; Delta_Expression : Node;
      Digits_Token          : Node; Digits_Expression : Node;
      Real_Range_Constraint : Node) return Node;

   function Defining_Character_Literal
     (Self : Node_Factory'Class; Character_Literal : Node) return Node;

   function Defining_Enumeration_Literal
     (Self : Node_Factory'Class; Identifier : Node) return Node;

   function Defining_Expanded_Unit_Name
     (Self : Node_Factory'Class; Defining_Prefix : Node; Dot_Token : Node;
      Defining_Selector : Node) return Node;

   function Defining_Identifier
     (Self : Node_Factory'Class; Identifier_Token : Node) return Node;

   function Defining_Operator_Symbol
     (Self : Node_Factory'Class; Operator_Symbol_Token : Node) return Node;

   function Delay_Statement
     (Self : Node_Factory'Class; Delay_Token : Node; Until_Token : Node;
      Delay_Expression : Node; Semicolon_Token : Node) return Node;

   function Delta_Constraint
     (Self : Node_Factory'Class; Delta_Token : Node; Delta_Expression : Node;
      Real_Range_Constraint : Node) return Node;

   function Derived_Record_Definition
     (Self : Node_Factory'Class; Abstract_Token : Node; Limited_Token : Node;
      New_Token       : Node; Parent_Subtype_Indication : Node;
      Progenitor_List : Node; With_Token : Node; Record_Definition : Node)
      return Node;

   function Derived_Type_Definition
     (Self : Node_Factory'Class; Abstract_Token : Node; Limited_Token : Node;
      New_Token : Node; Parent_Subtype_Indication : Node) return Node;

   function Digits_Constraint
     (Self : Node_Factory'Class; Digits_Token : Node; Digits_Expression : Node;
      Real_Range_Constraint : Node) return Node;

   function Discrete_Range_Attribute_Reference
     (Self : Node_Factory'Class; Range_Attribute : Node) return Node;

   function Discrete_Simple_Expression_Range
     (Self : Node_Factory'Class; Lower_Bound : Node; Double_Dot_Token : Node;
      Upper_Bound : Node) return Node;

   function Discrete_Subtype_Indication
     (Self               : Node_Factory'Class; Subtype_Mark : Node;
      Subtype_Constraint : Node) return Node;

   function Discrete_Subtype_Indication_Dr
     (Self               : Node_Factory'Class; Subtype_Mark : Node;
      Subtype_Constraint : Node) return Node;

   function Discriminant_Specification
     (Self             : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Not_Token : Node; Null_Token : Node; Object_Declaration_Subtype : Node;
      Assignment_Token : Node; Initialization_Expression : Node) return Node;

   function Element_Iterator_Specification
     (Self : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Subtype_Indication    : Node; Of_Token : Node; Reverse_Token : Node;
      Iteration_Scheme_Name : Node) return Node;

   function Else_Expression_Path
     (Self                 : Node_Factory'Class; Else_Token : Node;
      Dependent_Expression : Node) return Node;

   function Else_Path
     (Self                   : Node_Factory'Class; Else_Token : Node;
      Sequence_Of_Statements : Node) return Node;

   function Elsif_Expression_Path
     (Self                 : Node_Factory'Class; Elsif_Token : Node;
      Condition_Expression : Node; Then_Token : Node;
      Dependent_Expression : Node) return Node;

   function Elsif_Path
     (Self                   : Node_Factory'Class; Elsif_Token : Node;
      Condition_Expression   : Node; Then_Token : Node;
      Sequence_Of_Statements : Node) return Node;

   function Entry_Body
     (Self : Node_Factory'Class; Entry_Token : Node; Names : Node;
      Left_Parenthesis_Token  : Node; Entry_Index_Specification : Node;
      Right_Parenthesis_Token : Node; Lp_Token : Node;
      Parameter_Profile       : Node; Rp_Token : Node; When_Token : Node;
      Entry_Barrier : Node; Is_Token : Node; Body_Declarative_Items : Node;
      Begin_Token : Node; Body_Statements : Node; Exception_Token : Node;
      Exception_Handlers : Node; End_Token : Node; Identifier_Token : Node;
      Semicolon_Token         : Node) return Node;

   function Entry_Declaration
     (Self : Node_Factory'Class; Not_Token : Node; Overriding_Token : Node;
      Entry_Token : Node; Names : Node; Left_Parenthesis_Token : Node;
      Entry_Family_Definition : Node; Right_Parenthesis_Token : Node;
      Lp_Token : Node; Parameter_Profile : Node; Rp_Token : Node;
      Aspect_Specifications   : Node; Semicolon_Token : Node) return Node;

   function Entry_Index_Specification
     (Self     : Node_Factory'Class; For_Token : Node; Names : Node;
      In_Token : Node; Specification_Subtype_Definition : Node) return Node;

   function Enumeration_Literal_Specification
     (Self : Node_Factory'Class; Names : Node) return Node;

   function Enumeration_Type_Definition
     (Self                             : Node_Factory'Class; Left_Token : Node;
      Enumeration_Literal_Declarations : Node; Right_Token : Node) return Node;

   function Exception_Declaration
     (Self            : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Exception_Token : Node; Aspect_Specifications : Node;
      Semicolon_Token : Node) return Node;

   function Exception_Handler
     (Self                           : Node_Factory'Class; When_Token : Node;
      Choice_Parameter_Specification : Node; Colon_Token : Node;
      Exception_Choices : Node; Arrow_Token : Node; Handler_Statements : Node)
      return Node;

   function Exception_Renaming_Declaration
     (Self : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Exception_Token : Node; Renames_Token : Node; Renamed_Entity : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Exit_Statement
     (Self : Node_Factory'Class; Exit_Token : Node; Exit_Loop_Name : Node;
      When_Token : Node; Exit_Condition : Node; Semicolon_Token : Node)
      return Node;

   function Explicit_Dereference
     (Self      : Node_Factory'Class; Prefix : Node; Dot_Token : Node;
      All_Token : Node) return Node;

   function Extended_Return_Statement
     (Self                        : Node_Factory'Class; Return_Token : Node;
      Return_Object_Specification : Node; Do_Token : Node;
      Extended_Return_Statements  : Node; Exception_Token : Node;
      Exception_Handlers          : Node; End_Token : Node; End_Return : Node;
      Semicolon_Token             : Node) return Node;

   function Extension_Aggregate
     (Self                           : Node_Factory'Class; Left_Token : Node;
      Extension_Aggregate_Expression : Node; With_Token : Node;
      Record_Component_Associations  : Node; Right_Token : Node) return Node;

   function Floating_Point_Definition
     (Self : Node_Factory'Class; Digits_Token : Node; Digits_Expression : Node;
      Real_Range_Constraint : Node) return Node;

   function For_Loop_Statement
     (Self : Node_Factory'Class; Statement_Identifier : Node;
      Colon_Token                  : Node; For_Token : Node;
      Loop_Parameter_Specification : Node; Loop_Token : Node;
      Loop_Statements              : Node; End_Token : Node; End_Loop : Node;
      Identifier_Token             : Node; Semicolon_Token : Node) return Node;

   function Formal_Access_To_Function_Definition
     (Self : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Access_Token      : Node; Protected_Token : Node; Function_Token : Node;
      Lp_Token          : Node; Access_To_Subprogram_Parameter_Profile : Node;
      Rp_Token          : Node; Return_Token : Node; Return_Not_Token : Node;
      Return_Null_Token : Node; Access_To_Function_Result_Subtype : Node)
      return Node;

   function Formal_Access_To_Object_Definition
     (Self         : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Access_Token : Node; Constant_Token : Node; Subtype_Indication : Node)
      return Node;

   function Formal_Access_To_Procedure_Definition
     (Self         : Node_Factory'Class; Not_Token : Node; Null_Token : Node;
      Access_Token : Node; Protected_Token : Node; Procedure_Token : Node;
      Lp_Token     : Node; Access_To_Subprogram_Parameter_Profile : Node;
      Rp_Token     : Node) return Node;

   function Formal_Constrained_Array_Definition
     (Self : Node_Factory'Class; Array_Token : Node; Left_Token : Node;
      Discrete_Subtype_Definitions : Node; Right_Token : Node; Of_Token : Node;
      Array_Component_Definition   : Node) return Node;

   function Formal_Decimal_Fixed_Point_Definition
     (Self         : Node_Factory'Class; Delta_Token : Node; Delta_Box : Node;
      Digits_Token : Node; Digits_Box : Node) return Node;

   function Formal_Derived_Type_Definition
     (Self : Node_Factory'Class; Abstract_Token : Node; Limited_Token : Node;
      Synchronized_Token : Node; New_Token : Node; Subtype_Mark : Node;
      And_Token          : Node; Progenitor_List : Node; With_Token : Node;
      Private_Token      : Node; Aspect_Specifications : Node) return Node;

   function Formal_Discrete_Type_Definition
     (Self      : Node_Factory'Class; Left_Parenthesis_Token : Node;
      Box_Token : Node; Right_Parenthesis_Token : Node) return Node;

   function Formal_Floating_Point_Definition
     (Self : Node_Factory'Class; Digits_Token : Node; Box_Token : Node)
      return Node;

   function Formal_Function_Declaration
     (Self : Node_Factory'Class; With_Token : Node; Function_Token : Node;
      Names : Node; Lp_Token : Node; Parameter_Profile : Node; Rp_Token : Node;
      Return_Token : Node; Return_Not_Token : Node; Return_Null_Token : Node;
      Result_Subtype            : Node; Is_Token : Node; Abstract_Token : Node;
      Formal_Subprogram_Default : Node; Box_Token : Node;
      Aspect_Specifications     : Node; Semicolon_Token : Node) return Node;

   function Formal_Incomplete_Type_Declaration
     (Self              : Node_Factory'Class; Type_Token : Node; Names : Node;
      Discriminant_Part : Node; Is_Token : Node; Tagged_Token : Node;
      Semicolon_Token   : Node) return Node;

   function Formal_Interface_Type_Definition
     (Self : Node_Factory'Class; Kind_Token : Node; Interface_Token : Node;
      Progenitor_List : Node) return Node;

   function Formal_Modular_Type_Definition
     (Self : Node_Factory'Class; Mod_Token : Node; Box_Token : Node)
      return Node;

   function Formal_Object_Declaration
     (Self : Node_Factory'Class; Names : Node; Colon_Token : Node;
      In_Token : Node; Out_Token : Node; Not_Token : Node; Null_Token : Node;
      Object_Declaration_Subtype : Node; Assignment_Token : Node;
      Initialization_Expression  : Node; Aspect_Specifications : Node;
      Semicolon_Token            : Node) return Node;

   function Formal_Ordinary_Fixed_Point_Definition
     (Self : Node_Factory'Class; Delta_Token : Node; Box_Token : Node)
      return Node;

   function Formal_Package_Declaration
     (Self : Node_Factory'Class; With_Token : Node; Package_Token : Node;
      Names                 : Node; Is_Token : Node; New_Token : Node;
      Generic_Unit_Name     : Node; Left_Parenthesis_Token : Node;
      Generic_Actual_Part   : Node; Right_Parenthesis_Token : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Formal_Private_Type_Definition
     (Self : Node_Factory'Class; Abstract_Token : Node; Tagged_Token : Node;
      Limited_Token : Node; Private_Token : Node) return Node;

   function Formal_Procedure_Declaration
     (Self : Node_Factory'Class; With_Token : Node; Procedure_Token : Node;
      Names : Node; Lp_Token : Node; Parameter_Profile : Node; Rp_Token : Node;
      Is_Token              : Node; Abstract_Token : Node; Box_Token : Node;
      Null_Token            : Node; Formal_Subprogram_Default : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Formal_Signed_Integer_Type_Definition
     (Self : Node_Factory'Class; Range_Token : Node; Box_Token : Node)
      return Node;

   function Formal_Type_Declaration
     (Self : Node_Factory'Class; Type_Token : Node; Names : Node;
      Discriminant_Part : Node; Is_Token : Node; Type_Declaration_View : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Formal_Unconstrained_Array_Definition
     (Self : Node_Factory'Class; Array_Token : Node; Left_Token : Node;
      Index_Subtype_Definitions  : Node; Right_Token : Node; Of_Token : Node;
      Array_Component_Definition : Node) return Node;

   function Full_Type_Declaration
     (Self : Node_Factory'Class; Type_Token : Node; Names : Node;
      Discriminant_Part : Node; Is_Token : Node; Type_Declaration_View : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Function_Body
     (Self : Node_Factory'Class; Not_Token : Node; Overriding_Token : Node;
      Function_Token         : Node; Names : Node; Lp_Token : Node;
      Parameter_Profile      : Node; Rp_Token : Node; Return_Token : Node;
      Return_Not_Token : Node; Return_Null_Token : Node; Result_Subtype : Node;
      Aspect_Specifications  : Node; Is_Token : Node;
      Body_Declarative_Items : Node; Begin_Token : Node;
      Body_Statements        : Node; Exception_Token : Node;
      Exception_Handlers     : Node; End_Token : Node; End_Name : Node;
      Semicolon_Token        : Node) return Node;

   function Function_Call
     (Self                     : Node_Factory'Class; Prefix : Node;
      Function_Call_Parameters : Node) return Node;

   function Function_Declaration
     (Self : Node_Factory'Class; Not_Token : Node; Overriding_Token : Node;
      Function_Token        : Node; Names : Node; Lp_Token : Node;
      Parameter_Profile     : Node; Rp_Token : Node; Return_Token : Node;
      Return_Not_Token : Node; Return_Null_Token : Node; Result_Subtype : Node;
      Is_Token : Node; Abstract_Token : Node; Result_Expression : Node;
      Renames_Token : Node; Renamed_Entity : Node; Separate_Token : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Function_Instantiation
     (Self : Node_Factory'Class; Not_Token : Node; Overriding_Token : Node;
      Function_Token : Node; Names : Node; Is_Token : Node; New_Token : Node;
      Generic_Unit_Name     : Node; Left_Parenthesis_Token : Node;
      Generic_Actual_Part   : Node; Right_Parenthesis_Token : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Generalized_Iterator_Specification
     (Self          : Node_Factory'Class; Names : Node; In_Token : Node;
      Reverse_Token : Node; Iteration_Scheme_Name : Node) return Node;

   function Generic_Association
     (Self : Node_Factory'Class; Formal_Parameter : Node; Arrow_Token : Node;
      Actual_Parameter : Node; Box_Token : Node) return Node;

   function Generic_Function_Declaration
     (Self                : Node_Factory'Class; Generic_Token : Node;
      Generic_Formal_Part : Node; Function_Token : Node; Names : Node;
      Lp_Token            : Node; Parameter_Profile : Node; Rp_Token : Node;
      Return_Token : Node; Return_Not_Token : Node; Return_Null_Token : Node;
      Result_Subtype      : Node; Aspect_Specifications : Node;
      Semicolon_Token     : Node) return Node;

   function Generic_Function_Renaming
     (Self                  : Node_Factory'Class; Generic_Token : Node;
      Generic_Formal_Part   : Node; Function_Token : Node; Names : Node;
      Renames_Token         : Node; Renamed_Entity : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Generic_Package_Declaration
     (Self : Node_Factory'Class; Generic_Token : Node;
      Generic_Formal_Part : Node; Package_Token : Node; Names : Node;
      Aspect_Specifications          : Node; Is_Token : Node;
      Visible_Part_Declarative_Items : Node; Private_Token : Node;
      Private_Part_Declarative_Items : Node; End_Token : Node; End_Name : Node;
      Semicolon_Token                : Node) return Node;

   function Generic_Package_Renaming
     (Self                  : Node_Factory'Class; Generic_Token : Node;
      Generic_Formal_Part   : Node; Package_Token : Node; Names : Node;
      Renames_Token         : Node; Renamed_Entity : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Generic_Procedure_Declaration
     (Self                  : Node_Factory'Class; Generic_Token : Node;
      Generic_Formal_Part   : Node; Procedure_Token : Node; Names : Node;
      Lp_Token              : Node; Parameter_Profile : Node; Rp_Token : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Generic_Procedure_Renaming
     (Self                  : Node_Factory'Class; Generic_Token : Node;
      Generic_Formal_Part   : Node; Procedure_Token : Node; Names : Node;
      Renames_Token         : Node; Renamed_Entity : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Goto_Statement
     (Self : Node_Factory'Class; Exit_Token : Node; Goto_Label : Node;
      Semicolon_Token : Node) return Node;

   function Identifier
     (Self : Node_Factory'Class; Identifier_Token : Node) return Node;

   function If_Expression
     (Self : Node_Factory'Class; Expression_Paths : Node) return Node;

   function If_Expression_Path
     (Self : Node_Factory'Class; If_Token : Node; Condition_Expression : Node;
      Then_Token : Node; Dependent_Expression : Node) return Node;

   function If_Path
     (Self : Node_Factory'Class; If_Token : Node; Condition_Expression : Node;
      Then_Token : Node; Sequence_Of_Statements : Node) return Node;

   function If_Statement
     (Self     : Node_Factory'Class; Statement_Paths : Node; End_Token : Node;
      If_Token : Node; Semicolon_Token : Node) return Node;

   function Incomplete_Type_Declaration
     (Self              : Node_Factory'Class; Type_Token : Node; Names : Node;
      Discriminant_Part : Node; Is_Token : Node; Type_Declaration_View : Node;
      Semicolon_Token   : Node) return Node;

   function Incomplete_Type_Definition
     (Self : Node_Factory'Class; Tagged_Token : Node) return Node;

   function Interface_Type_Definition
     (Self : Node_Factory'Class; Kind_Token : Node; Interface_Token : Node;
      Progenitor_List : Node) return Node;

   function Known_Discriminant_Part
     (Self          : Node_Factory'Class; Left_Parenthesis_Token : Node;
      Discriminants : Node; Right_Parenthesis_Token : Node) return Node;

   function Label_Decorator
     (Self                : Node_Factory'Class; Label_Names : Node;
      Unlabeled_Statement : Node) return Node;

   function Loop_Parameter_Specification
     (Self          : Node_Factory'Class; Names : Node; In_Token : Node;
      Reverse_Token : Node; Specification_Subtype_Definition : Node)
      return Node;

   function Loop_Statement
     (Self            : Node_Factory'Class; Statement_Identifier : Node;
      Colon_Token     : Node; Loop_Token : Node; Loop_Statements : Node;
      End_Token       : Node; End_Loop : Node; Identifier_Token : Node;
      Semicolon_Token : Node) return Node;

   function Membership_Test
     (Self      : Node_Factory'Class; Membership_Test_Expression : Node;
      Not_Token : Node; In_Token : Node; Membership_Test_Choices : Node)
      return Node;

   function Modular_Type_Definition
     (Self                  : Node_Factory'Class; Mod_Token : Node;
      Mod_Static_Expression : Node) return Node;

   function Null_Component
     (Self : Node_Factory'Class; Null_Token : Node; Semicolon_Token : Node)
      return Node;

   function Null_Literal
     (Self : Node_Factory'Class; Null_Literal_Token : Node) return Node;

   function Null_Record_Definition
     (Self : Node_Factory'Class; Null_Token : Node; Record_Token : Node)
      return Node;

   function Null_Statement
     (Self : Node_Factory'Class; Null_Token : Node; Semicolon_Token : Node)
      return Node;

   function Number_Declaration
     (Self : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Constant_Token            : Node; Assignment_Token : Node;
      Initialization_Expression : Node; Semicolon_Token : Node) return Node;

   function Numeric_Literal
     (Self : Node_Factory'Class; Numeric_Literal_Token : Node) return Node;

   function Object_Declaration
     (Self : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Aliased_Token              : Node; Constant_Token : Node;
      Object_Declaration_Subtype : Node; Assignment_Token : Node;
      Initialization_Expression  : Node; Aspect_Specifications : Node;
      Semicolon_Token            : Node) return Node;

   function Object_Renaming_Declaration
     (Self : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Not_Token : Node; Null_Token : Node; Object_Declaration_Subtype : Node;
      Renames_Token         : Node; Renamed_Entity : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Operator_Symbol
     (Self : Node_Factory'Class; Operator_Symbol_Token : Node) return Node;

   function Ordinary_Fixed_Point_Definition
     (Self : Node_Factory'Class; Delta_Token : Node; Delta_Expression : Node;
      Real_Range_Constraint : Node) return Node;

   function Others_Choice
     (Self : Node_Factory'Class; Others_Token : Node) return Node;

   function Package_Body
     (Self : Node_Factory'Class; Package_Token : Node; Body_Token : Node;
      Names : Node; Aspect_Specifications : Node; Is_Token : Node;
      Body_Declarative_Items : Node; Begin_Token : Node;
      Body_Statements        : Node; Exception_Token : Node;
      Exception_Handlers     : Node; End_Token : Node; End_Name : Node;
      Semicolon_Token        : Node) return Node;

   function Package_Body_Stub
     (Self : Node_Factory'Class; Package_Token : Node; Body_Token : Node;
      Names                 : Node; Is_Token : Node; Separate_Token : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Package_Declaration
     (Self : Node_Factory'Class; Package_Token : Node; Names : Node;
      Aspect_Specifications          : Node; Is_Token : Node;
      Visible_Part_Declarative_Items : Node; Private_Token : Node;
      Private_Part_Declarative_Items : Node; End_Token : Node; End_Name : Node;
      Semicolon_Token                : Node) return Node;

   function Package_Instantiation
     (Self : Node_Factory'Class; Package_Token : Node; Names : Node;
      Is_Token : Node; New_Token : Node; Generic_Unit_Name : Node;
      Left_Parenthesis_Token  : Node; Generic_Actual_Part : Node;
      Right_Parenthesis_Token : Node; Aspect_Specifications : Node;
      Semicolon_Token         : Node) return Node;

   function Package_Renaming_Declaration
     (Self : Node_Factory'Class; Package_Token : Node; Names : Node;
      Renames_Token         : Node; Renamed_Entity : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Parameter_Association
     (Self             : Node_Factory'Class; Formal_Parameter : Node;
      Actual_Parameter : Node) return Node;

   function Parameter_Specification
     (Self             : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Aliased_Token    : Node; In_Token : Node; Out_Token : Node;
      Not_Token : Node; Null_Token : Node; Object_Declaration_Subtype : Node;
      Assignment_Token : Node; Initialization_Expression : Node) return Node;

   function Parenthesized_Expression
     (Self                     : Node_Factory'Class; Left_Token : Node;
      Expression_Parenthesized : Node; Right_Token : Node) return Node;

   function Pragma_Argument_Association
     (Self : Node_Factory'Class; Formal_Parameter : Node; Arrow_Token : Node;
      Actual_Parameter : Node) return Node;

   function Pragma_Node
     (Self : Node_Factory'Class; Pragma_Token : Node; Formal_Parameter : Node;
      Left_Token  : Node; Pragma_Argument_Associations : Node;
      Right_Token : Node; Semicolon_Token : Node) return Node;

   function Private_Extension_Declaration
     (Self : Node_Factory'Class; Type_Token : Node; Names : Node;
      Discriminant_Part : Node; Is_Token : Node; Type_Declaration_View : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Private_Extension_Definition
     (Self : Node_Factory'Class; Abstract_Token : Node; Limited_Token : Node;
      Synchronized_Token          : Node; New_Token : Node;
      Ancestor_Subtype_Indication : Node; Progenitor_List : Node;
      With_Token                  : Node; Private_Token : Node) return Node;

   function Private_Type_Declaration
     (Self : Node_Factory'Class; Type_Token : Node; Names : Node;
      Discriminant_Part : Node; Is_Token : Node; Type_Declaration_View : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Private_Type_Definition
     (Self : Node_Factory'Class; Abstract_Token : Node; Tagged_Token : Node;
      Limited_Token : Node; Private_Token : Node) return Node;

   function Procedure_Body
     (Self : Node_Factory'Class; Not_Token : Node; Overriding_Token : Node;
      Procedure_Token    : Node; Names : Node; Lp_Token : Node;
      Parameter_Profile  : Node; Rp_Token : Node; Aspect_Specifications : Node;
      Is_Token : Node; Body_Declarative_Items : Node; Begin_Token : Node;
      Body_Statements    : Node; Exception_Token : Node;
      Exception_Handlers : Node; End_Token : Node; End_Name : Node;
      Semicolon_Token    : Node) return Node;

   function Procedure_Call_Statement
     (Self : Node_Factory'Class; Function_Call : Node; Semicolon_Token : Node)
      return Node;

   function Procedure_Declaration
     (Self : Node_Factory'Class; Not_Token : Node; Overriding_Token : Node;
      Procedure_Token   : Node; Names : Node; Lp_Token : Node;
      Parameter_Profile : Node; Rp_Token : Node; Is_Token : Node;
      Abstract_Token    : Node; Renames_Token : Node; Renamed_Entity : Node;
      Separate_Token    : Node; Aspect_Specifications : Node;
      Semicolon_Token   : Node) return Node;

   function Procedure_Instantiation
     (Self : Node_Factory'Class; Not_Token : Node; Overriding_Token : Node;
      Procedure_Token : Node; Names : Node; Is_Token : Node; New_Token : Node;
      Generic_Unit_Name     : Node; Left_Parenthesis_Token : Node;
      Generic_Actual_Part   : Node; Right_Parenthesis_Token : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Protected_Body
     (Self : Node_Factory'Class; Protected_Token : Node; Body_Token : Node;
      Names : Node; Aspect_Specifications : Node; Is_Token : Node;
      Protected_Operation_Items : Node; End_Token : Node;
      Identifier_Token          : Node; Semicolon_Token : Node) return Node;

   function Protected_Body_Stub
     (Self : Node_Factory'Class; Protected_Token : Node; Body_Token : Node;
      Names                 : Node; Is_Token : Node; Separate_Token : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Protected_Definition
     (Self             : Node_Factory'Class; Visible_Protected_Items : Node;
      Private_Token : Node; Private_Protected_Items : Node; End_Token : Node;
      Identifier_Token : Node) return Node;

   function Protected_Type_Declaration
     (Self : Node_Factory'Class; Protected_Token : Node; Type_Token : Node;
      Names : Node; Discriminant_Part : Node; Aspect_Specifications : Node;
      Is_Token   : Node; New_Token : Node; Progenitor_List : Node;
      With_Token : Node; Type_Declaration_View : Node; Semicolon_Token : Node)
      return Node;

   function Qualified_Expression
     (Self : Node_Factory'Class; Converted_Or_Qualified_Subtype_Mark : Node;
      Apostrophe_Token                  : Node; Left_Parenthesis_Token : Node;
      Converted_Or_Qualified_Expression : Node; Right_Parenthesis_Token : Node)
      return Node;

   function Quantified_Expression
     (Self : Node_Factory'Class; For_Token : Node; Quantifier_Token : Node;
      Iterator_Specification : Node; Arrow_Token : Node; Predicate : Node)
      return Node;

   function Raise_Statement
     (Self : Node_Factory'Class; Raise_Token : Node; Raised_Exception : Node;
      With_Token      : Node; Raise_Statement_Message : Node;
      Semicolon_Token : Node) return Node;

   function Range_Attribute_Reference
     (Self : Node_Factory'Class; Range_Attribute : Node) return Node;

   function Range_Attribute_Reference_Dr
     (Self : Node_Factory'Class; Range_Attribute : Node) return Node;

   function Record_Aggregate
     (Self : Node_Factory'Class; Associations : Node) return Node;

   function Record_Definition
     (Self : Node_Factory'Class; Record_Token : Node; Record_Components : Node;
      End_Token : Node; End_Record_Token : Node) return Node;

   function Record_Representation_Clause
     (Self                       : Node_Factory'Class; For_Token : Node;
      Representation_Clause_Name : Node; Use_Token : Node; Record_Token : Node;
      At_Token : Node; Mod_Token : Node; Mod_Clause_Expression : Node;
      Mod_Semicolon : Node; Component_Clauses : Node; End_Token : Node;
      End_Record                 : Node; Semicolon_Token : Node) return Node;

   function Record_Type_Definition
     (Self : Node_Factory'Class; Abstract_Token : Node; Tagged_Token : Node;
      Limited_Token : Node; Record_Definition : Node) return Node;

   function Requeue_Statement
     (Self               : Node_Factory'Class; Requeue_Token : Node;
      Requeue_Entry_Name : Node; With_Token : Node; Abort_Token : Node;
      Semicolon_Token    : Node) return Node;

   function Return_Object_Specification
     (Self : Node_Factory'Class; Names : Node; Colon_Token : Node;
      Aliased_Token              : Node; Constant_Token : Node;
      Object_Declaration_Subtype : Node; Assignment_Token : Node;
      Initialization_Expression  : Node) return Node;

   function Root_Type_Definition
     (Self : Node_Factory'Class; Dummy_Token : Node) return Node;

   function Scalar_Subtype_Indication
     (Self : Node_Factory'Class; Subtype_Mark : Node; Scalar_Constraint : Node)
      return Node;

   function Select_Or_Path
     (Self  : Node_Factory'Class; Or_Token : Node; When_Token : Node;
      Guard : Node; Arrow_Token : Node; Sequence_Of_Statements : Node)
      return Node;

   function Selected_Component
     (Self     : Node_Factory'Class; Prefix : Node; Dot_Token : Node;
      Selector : Node) return Node;

   function Selected_Identifier
     (Self     : Node_Factory'Class; Prefix : Node; Dot_Token : Node;
      Selector : Node) return Node;

   function Selective_Accept
     (Self                      : Node_Factory'Class; Select_Token : Node;
      Selective_Statement_Paths : Node; End_Token : Node; End_Select : Node;
      Semicolon_Token           : Node) return Node;

   function Short_Circuit
     (Self                                    : Node_Factory'Class;
      Short_Circuit_Operation_Left_Expression : Node; And_Token : Node;
      Then_Token : Node; Short_Circuit_Operation_Right_Expression : Node)
      return Node;

   function Signed_Integer_Type_Definition
     (Self : Node_Factory'Class; Range_Token : Node; Integer_Constraint : Node)
      return Node;

   function Simple_Expression_Range
     (Self : Node_Factory'Class; Lower_Bound : Node; Double_Dot_Token : Node;
      Upper_Bound : Node) return Node;

   function Simple_Expression_Range_Dr
     (Self : Node_Factory'Class; Lower_Bound : Node; Double_Dot_Token : Node;
      Upper_Bound : Node) return Node;

   function Simple_Return_Statement
     (Self : Node_Factory'Class; Return_Token : Node; Return_Expression : Node;
      Semicolon_Token : Node) return Node;

   function Single_Protected_Declaration
     (Self : Node_Factory'Class; Protected_Token : Node; Names : Node;
      Aspect_Specifications      : Node; Is_Token : Node; New_Token : Node;
      Progenitor_List            : Node; With_Token : Node;
      Object_Declaration_Subtype : Node; Semicolon_Token : Node) return Node;

   function Single_Task_Declaration
     (Self : Node_Factory'Class; Task_Token : Node; Names : Node;
      Aspect_Specifications      : Node; Is_Token : Node; New_Token : Node;
      Progenitor_List            : Node; With_Token : Node;
      Object_Declaration_Subtype : Node; Semicolon_Token : Node) return Node;

   function String_Literal
     (Self : Node_Factory'Class; String_Literal_Token : Node) return Node;

   function Subtype_Declaration
     (Self : Node_Factory'Class; Subtype_Token : Node; Names : Node;
      Is_Token              : Node; Type_Declaration_View : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Subunit
     (Self             : Node_Factory'Class; Context_Clause_Elements : Node;
      Separate_Token   : Node; Left_Parenthesis_Token : Node;
      Parent_Unit_Name : Node; Right_Parenthesis_Token : Node;
      Unit_Declaration : Node) return Node;

   function Task_Body
     (Self : Node_Factory'Class; Task_Token : Node; Body_Token : Node;
      Names : Node; Aspect_Specifications : Node; Is_Token : Node;
      Body_Declarative_Items : Node; Begin_Token : Node;
      Body_Statements        : Node; Exception_Token : Node;
      Exception_Handlers     : Node; End_Token : Node; Identifier_Token : Node;
      Semicolon_Token        : Node) return Node;

   function Task_Body_Stub
     (Self : Node_Factory'Class; Task_Token : Node; Body_Token : Node;
      Names                 : Node; Is_Token : Node; Separate_Token : Node;
      Aspect_Specifications : Node; Semicolon_Token : Node) return Node;

   function Task_Definition
     (Self             : Node_Factory'Class; Visible_Task_Items : Node;
      Private_Token    : Node; Private_Task_Items : Node; End_Token : Node;
      Identifier_Token : Node) return Node;

   function Task_Type_Declaration
     (Self       : Node_Factory'Class; Task_Token : Node; Type_Token : Node;
      Names : Node; Discriminant_Part : Node; Aspect_Specifications : Node;
      Is_Token   : Node; New_Token : Node; Progenitor_List : Node;
      With_Token : Node; Type_Declaration_View : Node; Semicolon_Token : Node)
      return Node;

   function Terminate_Alternative_Statement
     (Self            : Node_Factory'Class; Terminate_Token : Node;
      Semicolon_Token : Node) return Node;

   function Then_Abort_Path
     (Self : Node_Factory'Class; Then_Token : Node; Abort_Token : Node;
      Sequence_Of_Statements : Node) return Node;

   function Unconstrained_Array_Definition
     (Self : Node_Factory'Class; Array_Token : Node; Left_Token : Node;
      Index_Subtype_Definitions  : Node; Right_Token : Node; Of_Token : Node;
      Array_Component_Definition : Node) return Node;

   function Unknown_Discriminant_Part
     (Self        : Node_Factory'Class; Left_Token : Node; Box_Token : Node;
      Right_Token : Node) return Node;

   function Use_Package_Clause
     (Self : Node_Factory'Class; Use_Token : Node; Clause_Names : Node;
      Semicolon_Token : Node) return Node;

   function Use_Type_Clause
     (Self       : Node_Factory'Class; Use_Token : Node; All_Token : Node;
      Type_Token : Node; Type_Clause_Names : Node; Semicolon_Token : Node)
      return Node;

   function Variant
     (Self : Node_Factory'Class; When_Token : Node; Variant_Choices : Node;
      Arrow_Token : Node; Record_Components : Node) return Node;

   function Variant_Part
     (Self                     : Node_Factory'Class; Case_Token : Node;
      Discriminant_Direct_Name : Node; Is_Token : Node; Variants : Node;
      End_Token : Node; End_Case_Token : Node; Semicolon_Token : Node)
      return Node;

   function While_Loop_Statement
     (Self        : Node_Factory'Class; Statement_Identifier : Node;
      Colon_Token : Node; While_Token : Node; While_Condition : Node;
      Loop_Token  : Node; Loop_Statements : Node; End_Token : Node;
      End_Loop    : Node; Identifier_Token : Node; Semicolon_Token : Node)
      return Node;

   function With_Clause
     (Self : Node_Factory'Class; Limited_Token : Node; Private_Token : Node;
      With_Token : Node; With_Clause_Names : Node; Semicolon_Token : Node)
      return Node;

   ---------------------
   --  Node sequences --
   ---------------------

   function Aspect_Specification_Sequence
     (Self : Node_Factory'Class) return Node;

   function Association_Sequence (Self : Node_Factory'Class) return Node;

   function Basic_Declarative_Item_Sequence
     (Self : Node_Factory'Class) return Node;

   function Case_Expression_Path_Sequence
     (Self : Node_Factory'Class) return Node;

   function Case_Path_Sequence (Self : Node_Factory'Class) return Node;

   function Clause_Or_Pragma_Sequence (Self : Node_Factory'Class) return Node;

   function Compilation_Unit_Sequence (Self : Node_Factory'Class) return Node;

   function Component_Item_Sequence (Self : Node_Factory'Class) return Node;

   function Context_Item_Sequence (Self : Node_Factory'Class) return Node;

   function Declarative_Item_Sequence (Self : Node_Factory'Class) return Node;

   function Defining_Identifier_Sequence
     (Self : Node_Factory'Class) return Node;

   function Discrete_Choice_Sequence (Self : Node_Factory'Class) return Node;

   function Discrete_Subtype_Definition_Sequence
     (Self : Node_Factory'Class) return Node;

   function Discriminant_Specification_Sequence
     (Self : Node_Factory'Class) return Node;

   function Enumeration_Literal_Specification_Sequence
     (Self : Node_Factory'Class) return Node;

   function Exception_Choice_Sequence (Self : Node_Factory'Class) return Node;

   function Exception_Handler_Sequence (Self : Node_Factory'Class) return Node;

   function Generic_Association_Sequence
     (Self : Node_Factory'Class) return Node;

   function Generic_Formal_Sequence (Self : Node_Factory'Class) return Node;

   function If_Else_Expression_Path_Sequence
     (Self : Node_Factory'Class) return Node;

   function If_Elsif_Else_Path_Sequence
     (Self : Node_Factory'Class) return Node;

   function Membership_Choice_Sequence (Self : Node_Factory'Class) return Node;

   function Name_Sequence (Self : Node_Factory'Class) return Node;

   function Parameter_Specification_Sequence
     (Self : Node_Factory'Class) return Node;

   function Pragma_Argument_Association_Sequence
     (Self : Node_Factory'Class) return Node;

   function Program_Unit_Name_Sequence (Self : Node_Factory'Class) return Node;

   function Protected_Element_Declaration_Sequence
     (Self : Node_Factory'Class) return Node;

   function Protected_Operation_Declaration_Sequence
     (Self : Node_Factory'Class) return Node;

   function Protected_Operation_Item_Sequence
     (Self : Node_Factory'Class) return Node;

   function Select_Or_Else_Path_Sequence
     (Self : Node_Factory'Class) return Node;

   function Select_Then_Abort_Path_Sequence
     (Self : Node_Factory'Class) return Node;

   function Statement_Sequence (Self : Node_Factory'Class) return Node;

   function Subtype_Mark_Sequence (Self : Node_Factory'Class) return Node;

   function Task_Item_Sequence (Self : Node_Factory'Class) return Node;

   function Variant_Sequence (Self : Node_Factory'Class) return Node;

   ------------
   -- Append --
   ------------

   procedure Append_Aspect_Specification
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Association
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Basic_Declarative_Item
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Case_Expression_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Case_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Clause_Or_Pragma
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Compilation_Unit
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Component_Item
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Context_Item
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Declarative_Item
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Defining_Identifier
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Discrete_Choice
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Discrete_Subtype_Definition
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Discriminant_Specification
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Enumeration_Literal_Specification
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Exception_Choice
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Exception_Handler
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Generic_Association
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Generic_Formal
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_If_Else_Expression_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_If_Elsif_Else_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Membership_Choice
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Name
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Parameter_Specification
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Pragma_Argument_Association
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Program_Unit_Name
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Protected_Element_Declaration
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Protected_Operation_Declaration
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Protected_Operation_Item
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Select_Or_Else_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Select_Then_Abort_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Statement
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Subtype_Mark
     (Self : Node_Factory'Class; List : in out Node; Item : Node);
   procedure Append_Variant
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   -------------
   -- Prepend --
   -------------

   procedure Prepend_Aspect_Specification
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Case_Expression_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Case_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Compilation_Unit
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Component_Item
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Declarative_Item
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Defining_Identifier
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Discrete_Choice
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Discrete_Subtype_Definition
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Discriminant_Specification
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Enumeration_Literal_Specification
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Exception_Choice
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Generic_Association
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_If_Else_Expression_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_If_Elsif_Else_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Membership_Choice
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Name
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Parameter_Specification
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Pragma_Argument_Association
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Statement
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Subtype_Mark
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Association
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Select_Or_Else_Path
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Exception_Handler
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Task_Item
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Program_Unit_Name
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   procedure Prepend_Variant
     (Self : Node_Factory'Class; List : in out Node; Item : Node);

   -------------------
   -- Special cases --
   -------------------

   function To_Defining_Program_Unit_Name
     (Self : Node_Factory'Class; Selected_Identifier : Node) return Node;
   --  Convert selected_identifier to defining_program_unit_name

   function Infix_Call
     (Self : Node_Factory'Class; Prefix, Left, Right : Node) return Node;

   function To_Aggregate_Or_Expression
     (Self : Node_Factory'Class; Association_List : Node) return Node;
   --  If Value is (X) return Parenthesized_Expression else
   --  return Record_Aggregate

   function To_Subtype_Indication
     (Self       : Node_Factory'Class;
      Not_Token  : Node;
      Null_Token : Node;
      Mark       : Node;
      Constraint : Node)
      return Node;

private
   type Node is null record;
   type Node_Factory is tagged limited null record;

   None     : constant Node := (null record);
   No_Token : constant Node := (null record);

end Program.Parsers.Nodes;
