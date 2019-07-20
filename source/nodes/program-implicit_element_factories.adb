--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Nodes.Pragmas;
with Program.Nodes.Defining_Identifiers;
with Program.Nodes.Defining_Character_Literals;
with Program.Nodes.Defining_Operator_Symbols;
with Program.Nodes.Defining_Expanded_Names;
with Program.Nodes.Type_Declarations;
with Program.Nodes.Task_Type_Declarations;
with Program.Nodes.Protected_Type_Declarations;
with Program.Nodes.Subtype_Declarations;
with Program.Nodes.Object_Declarations;
with Program.Nodes.Single_Task_Declarations;
with Program.Nodes.Single_Protected_Declarations;
with Program.Nodes.Number_Declarations;
with Program.Nodes.Enumeration_Literal_Specifications;
with Program.Nodes.Discriminant_Specifications;
with Program.Nodes.Component_Declarations;
with Program.Nodes.Loop_Parameter_Specifications;
with Program.Nodes.Generalized_Iterator_Specifications;
with Program.Nodes.Element_Iterator_Specifications;
with Program.Nodes.Procedure_Declarations;
with Program.Nodes.Function_Declarations;
with Program.Nodes.Parameter_Specifications;
with Program.Nodes.Procedure_Body_Declarations;
with Program.Nodes.Function_Body_Declarations;
with Program.Nodes.Return_Object_Specifications;
with Program.Nodes.Package_Declarations;
with Program.Nodes.Package_Body_Declarations;
with Program.Nodes.Object_Renaming_Declarations;
with Program.Nodes.Exception_Renaming_Declarations;
with Program.Nodes.Procedure_Renaming_Declarations;
with Program.Nodes.Function_Renaming_Declarations;
with Program.Nodes.Package_Renaming_Declarations;
with Program.Nodes.Generic_Package_Renaming_Declarations;
with Program.Nodes.Generic_Procedure_Renaming_Declarations;
with Program.Nodes.Generic_Function_Renaming_Declarations;
with Program.Nodes.Task_Body_Declarations;
with Program.Nodes.Protected_Body_Declarations;
with Program.Nodes.Entry_Declarations;
with Program.Nodes.Entry_Body_Declarations;
with Program.Nodes.Entry_Index_Specifications;
with Program.Nodes.Procedure_Body_Stubs;
with Program.Nodes.Function_Body_Stubs;
with Program.Nodes.Package_Body_Stubs;
with Program.Nodes.Task_Body_Stubs;
with Program.Nodes.Protected_Body_Stubs;
with Program.Nodes.Exception_Declarations;
with Program.Nodes.Choice_Parameter_Specifications;
with Program.Nodes.Generic_Package_Declarations;
with Program.Nodes.Generic_Procedure_Declarations;
with Program.Nodes.Generic_Function_Declarations;
with Program.Nodes.Package_Instantiations;
with Program.Nodes.Procedure_Instantiations;
with Program.Nodes.Function_Instantiations;
with Program.Nodes.Formal_Object_Declarations;
with Program.Nodes.Formal_Type_Declarations;
with Program.Nodes.Formal_Procedure_Declarations;
with Program.Nodes.Formal_Function_Declarations;
with Program.Nodes.Formal_Package_Declarations;
with Program.Nodes.Subtype_Indications;
with Program.Nodes.Component_Definitions;
with Program.Nodes.Unknown_Discriminant_Parts;
with Program.Nodes.Known_Discriminant_Parts;
with Program.Nodes.Record_Definitions;
with Program.Nodes.Null_Components;
with Program.Nodes.Variant_Parts;
with Program.Nodes.Variants;
with Program.Nodes.Others_Choices;
with Program.Nodes.Private_Type_Definitions;
with Program.Nodes.Private_Extension_Definitions;
with Program.Nodes.Incomplete_Type_Definitions;
with Program.Nodes.Task_Definitions;
with Program.Nodes.Protected_Definitions;
with Program.Nodes.Aspect_Specifications;
with Program.Nodes.Real_Range_Specifications;
with Program.Nodes.Numeric_Literals;
with Program.Nodes.String_Literals;
with Program.Nodes.Identifiers;
with Program.Nodes.Operator_Symbols;
with Program.Nodes.Character_Literals;
with Program.Nodes.Explicit_Dereferences;
with Program.Nodes.Function_Calls;
with Program.Nodes.Indexed_Components;
with Program.Nodes.Slices;
with Program.Nodes.Selected_Components;
with Program.Nodes.Attribute_References;
with Program.Nodes.Record_Aggregates;
with Program.Nodes.Extension_Aggregates;
with Program.Nodes.Array_Aggregates;
with Program.Nodes.Short_Circuit_Operations;
with Program.Nodes.Membership_Tests;
with Program.Nodes.Null_Literals;
with Program.Nodes.Parenthesized_Expressions;
with Program.Nodes.Raise_Expressions;
with Program.Nodes.Type_Conversions;
with Program.Nodes.Qualified_Expressions;
with Program.Nodes.Allocators;
with Program.Nodes.Case_Expressions;
with Program.Nodes.If_Expressions;
with Program.Nodes.Quantified_Expressions;
with Program.Nodes.Discriminant_Associations;
with Program.Nodes.Record_Component_Associations;
with Program.Nodes.Array_Component_Associations;
with Program.Nodes.Parameter_Associations;
with Program.Nodes.Formal_Package_Associations;
with Program.Nodes.Null_Statements;
with Program.Nodes.Assignment_Statements;
with Program.Nodes.If_Statements;
with Program.Nodes.Case_Statements;
with Program.Nodes.Loop_Statements;
with Program.Nodes.While_Loop_Statements;
with Program.Nodes.For_Loop_Statements;
with Program.Nodes.Block_Statements;
with Program.Nodes.Exit_Statements;
with Program.Nodes.Goto_Statements;
with Program.Nodes.Call_Statements;
with Program.Nodes.Simple_Return_Statements;
with Program.Nodes.Extended_Return_Statements;
with Program.Nodes.Accept_Statements;
with Program.Nodes.Requeue_Statements;
with Program.Nodes.Delay_Statements;
with Program.Nodes.Terminate_Alternative_Statements;
with Program.Nodes.Select_Statements;
with Program.Nodes.Abort_Statements;
with Program.Nodes.Raise_Statements;
with Program.Nodes.Code_Statements;
with Program.Nodes.Elsif_Paths;
with Program.Nodes.Case_Paths;
with Program.Nodes.Select_Paths;
with Program.Nodes.Case_Expression_Paths;
with Program.Nodes.Elsif_Expression_Paths;
with Program.Nodes.Use_Clauses;
with Program.Nodes.With_Clauses;
with Program.Nodes.Component_Clauses;
with Program.Nodes.Derived_Types;
with Program.Nodes.Derived_Record_Extensions;
with Program.Nodes.Enumeration_Types;
with Program.Nodes.Signed_Integer_Types;
with Program.Nodes.Modular_Types;
with Program.Nodes.Root_Types;
with Program.Nodes.Floating_Point_Types;
with Program.Nodes.Ordinary_Fixed_Point_Types;
with Program.Nodes.Decimal_Fixed_Point_Types;
with Program.Nodes.Unconstrained_Array_Types;
with Program.Nodes.Constrained_Array_Types;
with Program.Nodes.Record_Types;
with Program.Nodes.Interface_Types;
with Program.Nodes.Object_Access_Types;
with Program.Nodes.Procedure_Access_Types;
with Program.Nodes.Function_Access_Types;
with Program.Nodes.Formal_Private_Type_Definitions;
with Program.Nodes.Formal_Derived_Type_Definitions;
with Program.Nodes.Formal_Discrete_Type_Definitions;
with Program.Nodes.Formal_Signed_Integer_Type_Definitions;
with Program.Nodes.Formal_Modular_Type_Definitions;
with Program.Nodes.Formal_Floating_Point_Definitions;
with Program.Nodes.Formal_Ordinary_Fixed_Point_Definitions;
with Program.Nodes.Formal_Decimal_Fixed_Point_Definitions;
with Program.Nodes.Range_Attribute_References;
with Program.Nodes.Simple_Expression_Ranges;
with Program.Nodes.Digits_Constraints;
with Program.Nodes.Delta_Constraints;
with Program.Nodes.Index_Constraints;
with Program.Nodes.Discriminant_Constraints;
with Program.Nodes.Attribute_Definition_Clauses;
with Program.Nodes.Enumeration_Representation_Clauses;
with Program.Nodes.Record_Representation_Clauses;
with Program.Nodes.At_Clauses;
with Program.Nodes.Exception_Handlers;

package body Program.Implicit_Element_Factories is

   type Pragma_Access is not null access Program.Nodes.Pragmas.Implicit_Pragma;

   type Defining_Identifier_Access is
     not null access Program.Nodes.Defining_Identifiers
       .Implicit_Defining_Identifier;

   type Defining_Character_Literal_Access is
     not null access Program.Nodes.Defining_Character_Literals
       .Implicit_Defining_Character_Literal;

   type Defining_Operator_Symbol_Access is
     not null access Program.Nodes.Defining_Operator_Symbols
       .Implicit_Defining_Operator_Symbol;

   type Defining_Expanded_Name_Access is
     not null access Program.Nodes.Defining_Expanded_Names
       .Implicit_Defining_Expanded_Name;

   type Type_Declaration_Access is
     not null access Program.Nodes.Type_Declarations.Implicit_Type_Declaration;

   type Task_Type_Declaration_Access is
     not null access Program.Nodes.Task_Type_Declarations
       .Implicit_Task_Type_Declaration;

   type Protected_Type_Declaration_Access is
     not null access Program.Nodes.Protected_Type_Declarations
       .Implicit_Protected_Type_Declaration;

   type Subtype_Declaration_Access is
     not null access Program.Nodes.Subtype_Declarations
       .Implicit_Subtype_Declaration;

   type Object_Declaration_Access is
     not null access Program.Nodes.Object_Declarations
       .Implicit_Object_Declaration;

   type Single_Task_Declaration_Access is
     not null access Program.Nodes.Single_Task_Declarations
       .Implicit_Single_Task_Declaration;

   type Single_Protected_Declaration_Access is
     not null access Program.Nodes.Single_Protected_Declarations
       .Implicit_Single_Protected_Declaration;

   type Number_Declaration_Access is
     not null access Program.Nodes.Number_Declarations
       .Implicit_Number_Declaration;

   type Enumeration_Literal_Specification_Access is
     not null access Program.Nodes.Enumeration_Literal_Specifications
       .Implicit_Enumeration_Literal_Specification;

   type Discriminant_Specification_Access is
     not null access Program.Nodes.Discriminant_Specifications
       .Implicit_Discriminant_Specification;

   type Component_Declaration_Access is
     not null access Program.Nodes.Component_Declarations
       .Implicit_Component_Declaration;

   type Loop_Parameter_Specification_Access is
     not null access Program.Nodes.Loop_Parameter_Specifications
       .Implicit_Loop_Parameter_Specification;

   type Generalized_Iterator_Specification_Access is
     not null access Program.Nodes.Generalized_Iterator_Specifications
       .Implicit_Generalized_Iterator_Specification;

   type Element_Iterator_Specification_Access is
     not null access Program.Nodes.Element_Iterator_Specifications
       .Implicit_Element_Iterator_Specification;

   type Procedure_Declaration_Access is
     not null access Program.Nodes.Procedure_Declarations
       .Implicit_Procedure_Declaration;

   type Function_Declaration_Access is
     not null access Program.Nodes.Function_Declarations
       .Implicit_Function_Declaration;

   type Parameter_Specification_Access is
     not null access Program.Nodes.Parameter_Specifications
       .Implicit_Parameter_Specification;

   type Procedure_Body_Declaration_Access is
     not null access Program.Nodes.Procedure_Body_Declarations
       .Implicit_Procedure_Body_Declaration;

   type Function_Body_Declaration_Access is
     not null access Program.Nodes.Function_Body_Declarations
       .Implicit_Function_Body_Declaration;

   type Return_Object_Specification_Access is
     not null access Program.Nodes.Return_Object_Specifications
       .Implicit_Return_Object_Specification;

   type Package_Declaration_Access is
     not null access Program.Nodes.Package_Declarations
       .Implicit_Package_Declaration;

   type Package_Body_Declaration_Access is
     not null access Program.Nodes.Package_Body_Declarations
       .Implicit_Package_Body_Declaration;

   type Object_Renaming_Declaration_Access is
     not null access Program.Nodes.Object_Renaming_Declarations
       .Implicit_Object_Renaming_Declaration;

   type Exception_Renaming_Declaration_Access is
     not null access Program.Nodes.Exception_Renaming_Declarations
       .Implicit_Exception_Renaming_Declaration;

   type Procedure_Renaming_Declaration_Access is
     not null access Program.Nodes.Procedure_Renaming_Declarations
       .Implicit_Procedure_Renaming_Declaration;

   type Function_Renaming_Declaration_Access is
     not null access Program.Nodes.Function_Renaming_Declarations
       .Implicit_Function_Renaming_Declaration;

   type Package_Renaming_Declaration_Access is
     not null access Program.Nodes.Package_Renaming_Declarations
       .Implicit_Package_Renaming_Declaration;

   type Generic_Package_Renaming_Declaration_Access is
     not null access Program.Nodes.Generic_Package_Renaming_Declarations
       .Implicit_Generic_Package_Renaming_Declaration;

   type Generic_Procedure_Renaming_Declaration_Access is
     not null access Program.Nodes.Generic_Procedure_Renaming_Declarations
       .Implicit_Generic_Procedure_Renaming_Declaration;

   type Generic_Function_Renaming_Declaration_Access is
     not null access Program.Nodes.Generic_Function_Renaming_Declarations
       .Implicit_Generic_Function_Renaming_Declaration;

   type Task_Body_Declaration_Access is
     not null access Program.Nodes.Task_Body_Declarations
       .Implicit_Task_Body_Declaration;

   type Protected_Body_Declaration_Access is
     not null access Program.Nodes.Protected_Body_Declarations
       .Implicit_Protected_Body_Declaration;

   type Entry_Declaration_Access is
     not null access Program.Nodes.Entry_Declarations
       .Implicit_Entry_Declaration;

   type Entry_Body_Declaration_Access is
     not null access Program.Nodes.Entry_Body_Declarations
       .Implicit_Entry_Body_Declaration;

   type Entry_Index_Specification_Access is
     not null access Program.Nodes.Entry_Index_Specifications
       .Implicit_Entry_Index_Specification;

   type Procedure_Body_Stub_Access is
     not null access Program.Nodes.Procedure_Body_Stubs
       .Implicit_Procedure_Body_Stub;

   type Function_Body_Stub_Access is
     not null access Program.Nodes.Function_Body_Stubs
       .Implicit_Function_Body_Stub;

   type Package_Body_Stub_Access is
     not null access Program.Nodes.Package_Body_Stubs
       .Implicit_Package_Body_Stub;

   type Task_Body_Stub_Access is
     not null access Program.Nodes.Task_Body_Stubs.Implicit_Task_Body_Stub;

   type Protected_Body_Stub_Access is
     not null access Program.Nodes.Protected_Body_Stubs
       .Implicit_Protected_Body_Stub;

   type Exception_Declaration_Access is
     not null access Program.Nodes.Exception_Declarations
       .Implicit_Exception_Declaration;

   type Choice_Parameter_Specification_Access is
     not null access Program.Nodes.Choice_Parameter_Specifications
       .Implicit_Choice_Parameter_Specification;

   type Generic_Package_Declaration_Access is
     not null access Program.Nodes.Generic_Package_Declarations
       .Implicit_Generic_Package_Declaration;

   type Generic_Procedure_Declaration_Access is
     not null access Program.Nodes.Generic_Procedure_Declarations
       .Implicit_Generic_Procedure_Declaration;

   type Generic_Function_Declaration_Access is
     not null access Program.Nodes.Generic_Function_Declarations
       .Implicit_Generic_Function_Declaration;

   type Package_Instantiation_Access is
     not null access Program.Nodes.Package_Instantiations
       .Implicit_Package_Instantiation;

   type Procedure_Instantiation_Access is
     not null access Program.Nodes.Procedure_Instantiations
       .Implicit_Procedure_Instantiation;

   type Function_Instantiation_Access is
     not null access Program.Nodes.Function_Instantiations
       .Implicit_Function_Instantiation;

   type Formal_Object_Declaration_Access is
     not null access Program.Nodes.Formal_Object_Declarations
       .Implicit_Formal_Object_Declaration;

   type Formal_Type_Declaration_Access is
     not null access Program.Nodes.Formal_Type_Declarations
       .Implicit_Formal_Type_Declaration;

   type Formal_Procedure_Declaration_Access is
     not null access Program.Nodes.Formal_Procedure_Declarations
       .Implicit_Formal_Procedure_Declaration;

   type Formal_Function_Declaration_Access is
     not null access Program.Nodes.Formal_Function_Declarations
       .Implicit_Formal_Function_Declaration;

   type Formal_Package_Declaration_Access is
     not null access Program.Nodes.Formal_Package_Declarations
       .Implicit_Formal_Package_Declaration;

   type Subtype_Indication_Access is
     not null access Program.Nodes.Subtype_Indications
       .Implicit_Subtype_Indication;

   type Component_Definition_Access is
     not null access Program.Nodes.Component_Definitions
       .Implicit_Component_Definition;

   type Unknown_Discriminant_Part_Access is
     not null access Program.Nodes.Unknown_Discriminant_Parts
       .Implicit_Unknown_Discriminant_Part;

   type Known_Discriminant_Part_Access is
     not null access Program.Nodes.Known_Discriminant_Parts
       .Implicit_Known_Discriminant_Part;

   type Record_Definition_Access is
     not null access Program.Nodes.Record_Definitions
       .Implicit_Record_Definition;

   type Null_Component_Access is
     not null access Program.Nodes.Null_Components.Implicit_Null_Component;

   type Variant_Part_Access is
     not null access Program.Nodes.Variant_Parts.Implicit_Variant_Part;

   type Variant_Access is
     not null access Program.Nodes.Variants.Implicit_Variant;

   type Others_Choice_Access is
     not null access Program.Nodes.Others_Choices.Implicit_Others_Choice;

   type Private_Type_Definition_Access is
     not null access Program.Nodes.Private_Type_Definitions
       .Implicit_Private_Type_Definition;

   type Private_Extension_Definition_Access is
     not null access Program.Nodes.Private_Extension_Definitions
       .Implicit_Private_Extension_Definition;

   type Incomplete_Type_Definition_Access is
     not null access Program.Nodes.Incomplete_Type_Definitions
       .Implicit_Incomplete_Type_Definition;

   type Task_Definition_Access is
     not null access Program.Nodes.Task_Definitions.Implicit_Task_Definition;

   type Protected_Definition_Access is
     not null access Program.Nodes.Protected_Definitions
       .Implicit_Protected_Definition;

   type Aspect_Specification_Access is
     not null access Program.Nodes.Aspect_Specifications
       .Implicit_Aspect_Specification;

   type Real_Range_Specification_Access is
     not null access Program.Nodes.Real_Range_Specifications
       .Implicit_Real_Range_Specification;

   type Numeric_Literal_Access is
     not null access Program.Nodes.Numeric_Literals.Implicit_Numeric_Literal;

   type String_Literal_Access is
     not null access Program.Nodes.String_Literals.Implicit_String_Literal;

   type Identifier_Access is
     not null access Program.Nodes.Identifiers.Implicit_Identifier;

   type Operator_Symbol_Access is
     not null access Program.Nodes.Operator_Symbols.Implicit_Operator_Symbol;

   type Character_Literal_Access is
     not null access Program.Nodes.Character_Literals
       .Implicit_Character_Literal;

   type Explicit_Dereference_Access is
     not null access Program.Nodes.Explicit_Dereferences
       .Implicit_Explicit_Dereference;

   type Function_Call_Access is
     not null access Program.Nodes.Function_Calls.Implicit_Function_Call;

   type Indexed_Component_Access is
     not null access Program.Nodes.Indexed_Components
       .Implicit_Indexed_Component;

   type Slice_Access is not null access Program.Nodes.Slices.Implicit_Slice;

   type Selected_Component_Access is
     not null access Program.Nodes.Selected_Components
       .Implicit_Selected_Component;

   type Attribute_Reference_Access is
     not null access Program.Nodes.Attribute_References
       .Implicit_Attribute_Reference;

   type Record_Aggregate_Access is
     not null access Program.Nodes.Record_Aggregates.Implicit_Record_Aggregate;

   type Extension_Aggregate_Access is
     not null access Program.Nodes.Extension_Aggregates
       .Implicit_Extension_Aggregate;

   type Array_Aggregate_Access is
     not null access Program.Nodes.Array_Aggregates.Implicit_Array_Aggregate;

   type Short_Circuit_Operation_Access is
     not null access Program.Nodes.Short_Circuit_Operations
       .Implicit_Short_Circuit_Operation;

   type Membership_Test_Access is
     not null access Program.Nodes.Membership_Tests.Implicit_Membership_Test;

   type Null_Literal_Access is
     not null access Program.Nodes.Null_Literals.Implicit_Null_Literal;

   type Parenthesized_Expression_Access is
     not null access Program.Nodes.Parenthesized_Expressions
       .Implicit_Parenthesized_Expression;

   type Raise_Expression_Access is
     not null access Program.Nodes.Raise_Expressions.Implicit_Raise_Expression;

   type Type_Conversion_Access is
     not null access Program.Nodes.Type_Conversions.Implicit_Type_Conversion;

   type Qualified_Expression_Access is
     not null access Program.Nodes.Qualified_Expressions
       .Implicit_Qualified_Expression;

   type Allocator_Access is
     not null access Program.Nodes.Allocators.Implicit_Allocator;

   type Case_Expression_Access is
     not null access Program.Nodes.Case_Expressions.Implicit_Case_Expression;

   type If_Expression_Access is
     not null access Program.Nodes.If_Expressions.Implicit_If_Expression;

   type Quantified_Expression_Access is
     not null access Program.Nodes.Quantified_Expressions
       .Implicit_Quantified_Expression;

   type Discriminant_Association_Access is
     not null access Program.Nodes.Discriminant_Associations
       .Implicit_Discriminant_Association;

   type Record_Component_Association_Access is
     not null access Program.Nodes.Record_Component_Associations
       .Implicit_Record_Component_Association;

   type Array_Component_Association_Access is
     not null access Program.Nodes.Array_Component_Associations
       .Implicit_Array_Component_Association;

   type Parameter_Association_Access is
     not null access Program.Nodes.Parameter_Associations
       .Implicit_Parameter_Association;

   type Formal_Package_Association_Access is
     not null access Program.Nodes.Formal_Package_Associations
       .Implicit_Formal_Package_Association;

   type Null_Statement_Access is
     not null access Program.Nodes.Null_Statements.Implicit_Null_Statement;

   type Assignment_Statement_Access is
     not null access Program.Nodes.Assignment_Statements
       .Implicit_Assignment_Statement;

   type If_Statement_Access is
     not null access Program.Nodes.If_Statements.Implicit_If_Statement;

   type Case_Statement_Access is
     not null access Program.Nodes.Case_Statements.Implicit_Case_Statement;

   type Loop_Statement_Access is
     not null access Program.Nodes.Loop_Statements.Implicit_Loop_Statement;

   type While_Loop_Statement_Access is
     not null access Program.Nodes.While_Loop_Statements
       .Implicit_While_Loop_Statement;

   type For_Loop_Statement_Access is
     not null access Program.Nodes.For_Loop_Statements
       .Implicit_For_Loop_Statement;

   type Block_Statement_Access is
     not null access Program.Nodes.Block_Statements.Implicit_Block_Statement;

   type Exit_Statement_Access is
     not null access Program.Nodes.Exit_Statements.Implicit_Exit_Statement;

   type Goto_Statement_Access is
     not null access Program.Nodes.Goto_Statements.Implicit_Goto_Statement;

   type Call_Statement_Access is
     not null access Program.Nodes.Call_Statements.Implicit_Call_Statement;

   type Simple_Return_Statement_Access is
     not null access Program.Nodes.Simple_Return_Statements
       .Implicit_Simple_Return_Statement;

   type Extended_Return_Statement_Access is
     not null access Program.Nodes.Extended_Return_Statements
       .Implicit_Extended_Return_Statement;

   type Accept_Statement_Access is
     not null access Program.Nodes.Accept_Statements.Implicit_Accept_Statement;

   type Requeue_Statement_Access is
     not null access Program.Nodes.Requeue_Statements
       .Implicit_Requeue_Statement;

   type Delay_Statement_Access is
     not null access Program.Nodes.Delay_Statements.Implicit_Delay_Statement;

   type Terminate_Alternative_Statement_Access is
     not null access Program.Nodes.Terminate_Alternative_Statements
       .Implicit_Terminate_Alternative_Statement;

   type Select_Statement_Access is
     not null access Program.Nodes.Select_Statements.Implicit_Select_Statement;

   type Abort_Statement_Access is
     not null access Program.Nodes.Abort_Statements.Implicit_Abort_Statement;

   type Raise_Statement_Access is
     not null access Program.Nodes.Raise_Statements.Implicit_Raise_Statement;

   type Code_Statement_Access is
     not null access Program.Nodes.Code_Statements.Implicit_Code_Statement;

   type Elsif_Path_Access is
     not null access Program.Nodes.Elsif_Paths.Implicit_Elsif_Path;

   type Case_Path_Access is
     not null access Program.Nodes.Case_Paths.Implicit_Case_Path;

   type Select_Path_Access is
     not null access Program.Nodes.Select_Paths.Implicit_Select_Path;

   type Case_Expression_Path_Access is
     not null access Program.Nodes.Case_Expression_Paths
       .Implicit_Case_Expression_Path;

   type Elsif_Expression_Path_Access is
     not null access Program.Nodes.Elsif_Expression_Paths
       .Implicit_Elsif_Expression_Path;

   type Use_Clause_Access is
     not null access Program.Nodes.Use_Clauses.Implicit_Use_Clause;

   type With_Clause_Access is
     not null access Program.Nodes.With_Clauses.Implicit_With_Clause;

   type Component_Clause_Access is
     not null access Program.Nodes.Component_Clauses.Implicit_Component_Clause;

   type Derived_Type_Access is
     not null access Program.Nodes.Derived_Types.Implicit_Derived_Type;

   type Derived_Record_Extension_Access is
     not null access Program.Nodes.Derived_Record_Extensions
       .Implicit_Derived_Record_Extension;

   type Enumeration_Type_Access is
     not null access Program.Nodes.Enumeration_Types.Implicit_Enumeration_Type;

   type Signed_Integer_Type_Access is
     not null access Program.Nodes.Signed_Integer_Types
       .Implicit_Signed_Integer_Type;

   type Modular_Type_Access is
     not null access Program.Nodes.Modular_Types.Implicit_Modular_Type;

   type Root_Type_Access is
     not null access Program.Nodes.Root_Types.Implicit_Root_Type;

   type Floating_Point_Type_Access is
     not null access Program.Nodes.Floating_Point_Types
       .Implicit_Floating_Point_Type;

   type Ordinary_Fixed_Point_Type_Access is
     not null access Program.Nodes.Ordinary_Fixed_Point_Types
       .Implicit_Ordinary_Fixed_Point_Type;

   type Decimal_Fixed_Point_Type_Access is
     not null access Program.Nodes.Decimal_Fixed_Point_Types
       .Implicit_Decimal_Fixed_Point_Type;

   type Unconstrained_Array_Type_Access is
     not null access Program.Nodes.Unconstrained_Array_Types
       .Implicit_Unconstrained_Array_Type;

   type Constrained_Array_Type_Access is
     not null access Program.Nodes.Constrained_Array_Types
       .Implicit_Constrained_Array_Type;

   type Record_Type_Access is
     not null access Program.Nodes.Record_Types.Implicit_Record_Type;

   type Interface_Type_Access is
     not null access Program.Nodes.Interface_Types.Implicit_Interface_Type;

   type Object_Access_Type_Access is
     not null access Program.Nodes.Object_Access_Types
       .Implicit_Object_Access_Type;

   type Procedure_Access_Type_Access is
     not null access Program.Nodes.Procedure_Access_Types
       .Implicit_Procedure_Access_Type;

   type Function_Access_Type_Access is
     not null access Program.Nodes.Function_Access_Types
       .Implicit_Function_Access_Type;

   type Formal_Private_Type_Definition_Access is
     not null access Program.Nodes.Formal_Private_Type_Definitions
       .Implicit_Formal_Private_Type_Definition;

   type Formal_Derived_Type_Definition_Access is
     not null access Program.Nodes.Formal_Derived_Type_Definitions
       .Implicit_Formal_Derived_Type_Definition;

   type Formal_Discrete_Type_Definition_Access is
     not null access Program.Nodes.Formal_Discrete_Type_Definitions
       .Implicit_Formal_Discrete_Type_Definition;

   type Formal_Signed_Integer_Type_Definition_Access is
     not null access Program.Nodes.Formal_Signed_Integer_Type_Definitions
       .Implicit_Formal_Signed_Integer_Type_Definition;

   type Formal_Modular_Type_Definition_Access is
     not null access Program.Nodes.Formal_Modular_Type_Definitions
       .Implicit_Formal_Modular_Type_Definition;

   type Formal_Floating_Point_Definition_Access is
     not null access Program.Nodes.Formal_Floating_Point_Definitions
       .Implicit_Formal_Floating_Point_Definition;

   type Formal_Ordinary_Fixed_Point_Definition_Access is
     not null access Program.Nodes.Formal_Ordinary_Fixed_Point_Definitions
       .Implicit_Formal_Ordinary_Fixed_Point_Definition;

   type Formal_Decimal_Fixed_Point_Definition_Access is
     not null access Program.Nodes.Formal_Decimal_Fixed_Point_Definitions
       .Implicit_Formal_Decimal_Fixed_Point_Definition;

   type Range_Attribute_Reference_Access is
     not null access Program.Nodes.Range_Attribute_References
       .Implicit_Range_Attribute_Reference;

   type Simple_Expression_Range_Access is
     not null access Program.Nodes.Simple_Expression_Ranges
       .Implicit_Simple_Expression_Range;

   type Digits_Constraint_Access is
     not null access Program.Nodes.Digits_Constraints
       .Implicit_Digits_Constraint;

   type Delta_Constraint_Access is
     not null access Program.Nodes.Delta_Constraints.Implicit_Delta_Constraint;

   type Index_Constraint_Access is
     not null access Program.Nodes.Index_Constraints.Implicit_Index_Constraint;

   type Discriminant_Constraint_Access is
     not null access Program.Nodes.Discriminant_Constraints
       .Implicit_Discriminant_Constraint;

   type Attribute_Definition_Clause_Access is
     not null access Program.Nodes.Attribute_Definition_Clauses
       .Implicit_Attribute_Definition_Clause;

   type Enumeration_Representation_Clause_Access is
     not null access Program.Nodes.Enumeration_Representation_Clauses
       .Implicit_Enumeration_Representation_Clause;

   type Record_Representation_Clause_Access is
     not null access Program.Nodes.Record_Representation_Clauses
       .Implicit_Record_Representation_Clause;

   type At_Clause_Access is
     not null access Program.Nodes.At_Clauses.Implicit_At_Clause;

   type Exception_Handler_Access is
     not null access Program.Nodes.Exception_Handlers
       .Implicit_Exception_Handler;

   function Create_Pragma
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Identifiers
         .Identifier_Access;
     Arguments            : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Pragmas.Pragma_Access is
      Result : constant Pragma_Access :=

          new (Self.Subpool) Program.Nodes.Pragmas.Implicit_Pragma'
            (Program.Nodes.Pragmas.Create
               (Name => Name, Arguments => Arguments,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Pragmas.Pragma_Access (Result);
   end Create_Pragma;

   function Create_Defining_Identifier
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
      Result : constant Defining_Identifier_Access :=

          new (Self.Subpool) Program.Nodes.Defining_Identifiers
            .Implicit_Defining_Identifier'
            (Program.Nodes.Defining_Identifiers.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
        (Result);
   end Create_Defining_Identifier;

   function Create_Defining_Character_Literal
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Defining_Character_Literals
          .Defining_Character_Literal_Access is
      Result : constant Defining_Character_Literal_Access :=

          new (Self.Subpool) Program.Nodes.Defining_Character_Literals
            .Implicit_Defining_Character_Literal'
            (Program.Nodes.Defining_Character_Literals.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Defining_Character_Literals
        .Defining_Character_Literal_Access
        (Result);
   end Create_Defining_Character_Literal;

   function Create_Defining_Operator_Symbol
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Defining_Operator_Symbols
          .Defining_Operator_Symbol_Access is
      Result : constant Defining_Operator_Symbol_Access :=

          new (Self.Subpool) Program.Nodes.Defining_Operator_Symbols
            .Implicit_Defining_Operator_Symbol'
            (Program.Nodes.Defining_Operator_Symbols.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Defining_Operator_Symbols
        .Defining_Operator_Symbol_Access
        (Result);
   end Create_Defining_Operator_Symbol;

   function Create_Defining_Expanded_Name
    (Self : Element_Factory;
     Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Selector             : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Defining_Expanded_Names
          .Defining_Expanded_Name_Access is
      Result : constant Defining_Expanded_Name_Access :=

          new (Self.Subpool) Program.Nodes.Defining_Expanded_Names
            .Implicit_Defining_Expanded_Name'
            (Program.Nodes.Defining_Expanded_Names.Create
               (Prefix => Prefix, Selector => Selector,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Defining_Expanded_Names
        .Defining_Expanded_Name_Access
        (Result);
   end Create_Defining_Expanded_Name;

   function Create_Type_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Discriminant_Part    : Program.Elements.Definitions.Definition_Access;
     Definition           : not null Program.Elements.Definitions
         .Definition_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Type_Declarations
          .Type_Declaration_Access is
      Result : constant Type_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Type_Declarations
            .Implicit_Type_Declaration'
            (Program.Nodes.Type_Declarations.Create
               (Name => Name, Discriminant_Part => Discriminant_Part,
                Definition => Definition, Aspects => Aspects,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Type_Declarations.Type_Declaration_Access
        (Result);
   end Create_Type_Declaration;

   function Create_Task_Type_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Discriminant_Part    : Program.Elements.Known_Discriminant_Parts
         .Known_Discriminant_Part_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Progenitors          : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Definition           : not null Program.Elements.Task_Definitions
         .Task_Definition_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Task_Type_Declarations
          .Task_Type_Declaration_Access is
      Result : constant Task_Type_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Task_Type_Declarations
            .Implicit_Task_Type_Declaration'
            (Program.Nodes.Task_Type_Declarations.Create
               (Name => Name, Discriminant_Part => Discriminant_Part,
                Aspects => Aspects, Progenitors => Progenitors,
                Definition => Definition,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Task_Type_Declarations
        .Task_Type_Declaration_Access
        (Result);
   end Create_Task_Type_Declaration;

   function Create_Protected_Type_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Discriminant_Part    : Program.Elements.Known_Discriminant_Parts
         .Known_Discriminant_Part_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Progenitors          : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Definition           : not null Program.Elements.Protected_Definitions
         .Protected_Definition_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Protected_Type_Declarations
          .Protected_Type_Declaration_Access is
      Result : constant Protected_Type_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Protected_Type_Declarations
            .Implicit_Protected_Type_Declaration'
            (Program.Nodes.Protected_Type_Declarations.Create
               (Name => Name, Discriminant_Part => Discriminant_Part,
                Aspects => Aspects, Progenitors => Progenitors,
                Definition => Definition,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Protected_Type_Declarations
        .Protected_Type_Declaration_Access
        (Result);
   end Create_Protected_Type_Declaration;

   function Create_Subtype_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Subtype_Indication   : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Subtype_Declarations
          .Subtype_Declaration_Access is
      Result : constant Subtype_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Subtype_Declarations
            .Implicit_Subtype_Declaration'
            (Program.Nodes.Subtype_Declarations.Create
               (Name => Name, Subtype_Indication => Subtype_Indication,
                Aspects => Aspects, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Subtype_Declarations.Subtype_Declaration_Access
        (Result);
   end Create_Subtype_Declaration;

   function Create_Object_Declaration
    (Self : Element_Factory;
     Names                     : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Object_Subtype            : not null Program.Elements.Definitions
         .Definition_Access;
     Initialization_Expression : Program.Elements.Expressions
         .Expression_Access;
     Aspects                   : not null Program.Elements
         .Aspect_Specifications.Aspect_Specification_Vector_Access;
     Has_Aliased               : Boolean := False;
     Has_Constant              : Boolean := False;
     Is_Part_Of_Implicit       : Boolean := False;
     Is_Part_Of_Inherited      : Boolean := False;
     Is_Part_Of_Instance       : Boolean := False)
      return not null Program.Elements.Object_Declarations
          .Object_Declaration_Access is
      Result : constant Object_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Object_Declarations
            .Implicit_Object_Declaration'
            (Program.Nodes.Object_Declarations.Create
               (Names => Names, Object_Subtype => Object_Subtype,
                Initialization_Expression => Initialization_Expression,
                Aspects => Aspects, Has_Aliased => Has_Aliased,
                Has_Constant => Has_Constant,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Object_Declarations.Object_Declaration_Access
        (Result);
   end Create_Object_Declaration;

   function Create_Single_Task_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Progenitors          : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Definition           : not null Program.Elements.Task_Definitions
         .Task_Definition_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Single_Task_Declarations
          .Single_Task_Declaration_Access is
      Result : constant Single_Task_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Single_Task_Declarations
            .Implicit_Single_Task_Declaration'
            (Program.Nodes.Single_Task_Declarations.Create
               (Name => Name, Aspects => Aspects, Progenitors => Progenitors,
                Definition => Definition,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Single_Task_Declarations
        .Single_Task_Declaration_Access
        (Result);
   end Create_Single_Task_Declaration;

   function Create_Single_Protected_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Progenitors          : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Definition           : not null Program.Elements.Protected_Definitions
         .Protected_Definition_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Single_Protected_Declarations
          .Single_Protected_Declaration_Access is
      Result : constant Single_Protected_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Single_Protected_Declarations
            .Implicit_Single_Protected_Declaration'
            (Program.Nodes.Single_Protected_Declarations.Create
               (Name => Name, Aspects => Aspects, Progenitors => Progenitors,
                Definition => Definition,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Single_Protected_Declarations
        .Single_Protected_Declaration_Access
        (Result);
   end Create_Single_Protected_Declaration;

   function Create_Number_Declaration
    (Self : Element_Factory;
     Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Number_Declarations
          .Number_Declaration_Access is
      Result : constant Number_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Number_Declarations
            .Implicit_Number_Declaration'
            (Program.Nodes.Number_Declarations.Create
               (Names => Names, Expression => Expression,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Number_Declarations.Number_Declaration_Access
        (Result);
   end Create_Number_Declaration;

   function Create_Enumeration_Literal_Specification
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Enumeration_Literal_Specifications
          .Enumeration_Literal_Specification_Access is
      Result : constant Enumeration_Literal_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Enumeration_Literal_Specifications
            .Implicit_Enumeration_Literal_Specification'
            (Program.Nodes.Enumeration_Literal_Specifications.Create
               (Name => Name, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Enumeration_Literal_Specifications
        .Enumeration_Literal_Specification_Access
        (Result);
   end Create_Enumeration_Literal_Specification;

   function Create_Discriminant_Specification
    (Self : Element_Factory;
     Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Object_Subtype       : not null Program.Elements.Element_Access;
     Default_Expression   : Program.Elements.Expressions.Expression_Access;
     Has_Not_Null         : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Discriminant_Specifications
          .Discriminant_Specification_Access is
      Result : constant Discriminant_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Discriminant_Specifications
            .Implicit_Discriminant_Specification'
            (Program.Nodes.Discriminant_Specifications.Create
               (Names => Names, Object_Subtype => Object_Subtype,
                Default_Expression => Default_Expression,
                Has_Not_Null => Has_Not_Null,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Discriminant_Specifications
        .Discriminant_Specification_Access
        (Result);
   end Create_Discriminant_Specification;

   function Create_Component_Declaration
    (Self : Element_Factory;
     Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Object_Subtype       : not null Program.Elements.Component_Definitions
         .Component_Definition_Access;
     Default_Expression   : Program.Elements.Expressions.Expression_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Component_Declarations
          .Component_Declaration_Access is
      Result : constant Component_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Component_Declarations
            .Implicit_Component_Declaration'
            (Program.Nodes.Component_Declarations.Create
               (Names => Names, Object_Subtype => Object_Subtype,
                Default_Expression => Default_Expression, Aspects => Aspects,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Component_Declarations
        .Component_Declaration_Access
        (Result);
   end Create_Component_Declaration;

   function Create_Loop_Parameter_Specification
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Definition           : not null Program.Elements
         .Discrete_Subtype_Definitions.Discrete_Subtype_Definition_Access;
     Has_Reverse          : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification_Access is
      Result : constant Loop_Parameter_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Loop_Parameter_Specifications
            .Implicit_Loop_Parameter_Specification'
            (Program.Nodes.Loop_Parameter_Specifications.Create
               (Name => Name, Definition => Definition,
                Has_Reverse => Has_Reverse,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Loop_Parameter_Specifications
        .Loop_Parameter_Specification_Access
        (Result);
   end Create_Loop_Parameter_Specification;

   function Create_Generalized_Iterator_Specification
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Iterator_Name        : not null Program.Elements.Expressions
         .Expression_Access;
     Has_Reverse          : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification_Access is
      Result : constant Generalized_Iterator_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Generalized_Iterator_Specifications
            .Implicit_Generalized_Iterator_Specification'
            (Program.Nodes.Generalized_Iterator_Specifications.Create
               (Name => Name, Iterator_Name => Iterator_Name,
                Has_Reverse => Has_Reverse,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Generalized_Iterator_Specifications
        .Generalized_Iterator_Specification_Access
        (Result);
   end Create_Generalized_Iterator_Specification;

   function Create_Element_Iterator_Specification
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Subtype_Indication   : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Iterable_Name        : not null Program.Elements.Expressions
         .Expression_Access;
     Has_Reverse          : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification_Access is
      Result : constant Element_Iterator_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Element_Iterator_Specifications
            .Implicit_Element_Iterator_Specification'
            (Program.Nodes.Element_Iterator_Specifications.Create
               (Name => Name, Subtype_Indication => Subtype_Indication,
                Iterable_Name => Iterable_Name, Has_Reverse => Has_Reverse,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Element_Iterator_Specifications
        .Element_Iterator_Specification_Access
        (Result);
   end Create_Element_Iterator_Specification;

   function Create_Procedure_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Parameters           : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Has_Not              : Boolean := False;
     Has_Overriding       : Boolean := False;
     Has_Abstract         : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Procedure_Declarations
          .Procedure_Declaration_Access is
      Result : constant Procedure_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Procedure_Declarations
            .Implicit_Procedure_Declaration'
            (Program.Nodes.Procedure_Declarations.Create
               (Name => Name, Parameters => Parameters, Aspects => Aspects,
                Has_Not => Has_Not, Has_Overriding => Has_Overriding,
                Has_Abstract => Has_Abstract,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Procedure_Declarations
        .Procedure_Declaration_Access
        (Result);
   end Create_Procedure_Declaration;

   function Create_Function_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Parameters           : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Result_Subtype       : not null Program.Elements.Element_Access;
     Result_Expression    : Program.Elements.Parenthesized_Expressions
         .Parenthesized_Expression_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Has_Not              : Boolean := False;
     Has_Overriding       : Boolean := False;
     Has_Abstract         : Boolean := False;
     Has_Not_Null         : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Function_Declarations
          .Function_Declaration_Access is
      Result : constant Function_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Function_Declarations
            .Implicit_Function_Declaration'
            (Program.Nodes.Function_Declarations.Create
               (Name => Name, Parameters => Parameters,
                Result_Subtype => Result_Subtype,
                Result_Expression => Result_Expression, Aspects => Aspects,
                Has_Not => Has_Not, Has_Overriding => Has_Overriding,
                Has_Abstract => Has_Abstract, Has_Not_Null => Has_Not_Null,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Function_Declarations.Function_Declaration_Access
        (Result);
   end Create_Function_Declaration;

   function Create_Parameter_Specification
    (Self : Element_Factory;
     Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Parameter_Subtype    : not null Program.Elements.Element_Access;
     Default_Expression   : Program.Elements.Expressions.Expression_Access;
     Has_Aliased          : Boolean := False;
     Has_In               : Boolean := False;
     Has_Out              : Boolean := False;
     Has_Not_Null         : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Parameter_Specifications
          .Parameter_Specification_Access is
      Result : constant Parameter_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Parameter_Specifications
            .Implicit_Parameter_Specification'
            (Program.Nodes.Parameter_Specifications.Create
               (Names => Names, Parameter_Subtype => Parameter_Subtype,
                Default_Expression => Default_Expression,
                Has_Aliased => Has_Aliased, Has_In => Has_In,
                Has_Out => Has_Out, Has_Not_Null => Has_Not_Null,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Parameter_Specifications
        .Parameter_Specification_Access
        (Result);
   end Create_Parameter_Specification;

   function Create_Procedure_Body_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Parameters           : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Declarations         : not null Program.Element_Vectors
         .Element_Vector_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Handlers   : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Name             : Program.Elements.Expressions.Expression_Access;
     Has_Not              : Boolean := False;
     Has_Overriding       : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Procedure_Body_Declarations
          .Procedure_Body_Declaration_Access is
      Result : constant Procedure_Body_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Procedure_Body_Declarations
            .Implicit_Procedure_Body_Declaration'
            (Program.Nodes.Procedure_Body_Declarations.Create
               (Name => Name, Parameters => Parameters, Aspects => Aspects,
                Declarations => Declarations, Statements => Statements,
                Exception_Handlers => Exception_Handlers, End_Name => End_Name,
                Has_Not => Has_Not, Has_Overriding => Has_Overriding,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Procedure_Body_Declarations
        .Procedure_Body_Declaration_Access
        (Result);
   end Create_Procedure_Body_Declaration;

   function Create_Function_Body_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Parameters           : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Result_Subtype       : not null Program.Elements.Element_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Declarations         : not null Program.Element_Vectors
         .Element_Vector_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Handlers   : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Name             : Program.Elements.Expressions.Expression_Access;
     Has_Not              : Boolean := False;
     Has_Overriding       : Boolean := False;
     Has_Not_Null         : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Function_Body_Declarations
          .Function_Body_Declaration_Access is
      Result : constant Function_Body_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Function_Body_Declarations
            .Implicit_Function_Body_Declaration'
            (Program.Nodes.Function_Body_Declarations.Create
               (Name => Name, Parameters => Parameters,
                Result_Subtype => Result_Subtype, Aspects => Aspects,
                Declarations => Declarations, Statements => Statements,
                Exception_Handlers => Exception_Handlers, End_Name => End_Name,
                Has_Not => Has_Not, Has_Overriding => Has_Overriding,
                Has_Not_Null => Has_Not_Null,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Function_Body_Declarations
        .Function_Body_Declaration_Access
        (Result);
   end Create_Function_Body_Declaration;

   function Create_Return_Object_Specification
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Object_Subtype       : not null Program.Elements.Element_Access;
     Expression           : Program.Elements.Expressions.Expression_Access;
     Has_Aliased          : Boolean := False;
     Has_Constant         : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Return_Object_Specifications
          .Return_Object_Specification_Access is
      Result : constant Return_Object_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Return_Object_Specifications
            .Implicit_Return_Object_Specification'
            (Program.Nodes.Return_Object_Specifications.Create
               (Name => Name, Object_Subtype => Object_Subtype,
                Expression => Expression, Has_Aliased => Has_Aliased,
                Has_Constant => Has_Constant,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Return_Object_Specifications
        .Return_Object_Specification_Access
        (Result);
   end Create_Return_Object_Specification;

   function Create_Package_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Visible_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     Private_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Name             : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Package_Declarations
          .Package_Declaration_Access is
      Result : constant Package_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Package_Declarations
            .Implicit_Package_Declaration'
            (Program.Nodes.Package_Declarations.Create
               (Name => Name, Aspects => Aspects,
                Visible_Declarations => Visible_Declarations,
                Private_Declarations => Private_Declarations,
                End_Name => End_Name,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Package_Declarations.Package_Declaration_Access
        (Result);
   end Create_Package_Declaration;

   function Create_Package_Body_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Declarations         : not null Program.Element_Vectors
         .Element_Vector_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Handlers   : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Name             : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Package_Body_Declarations
          .Package_Body_Declaration_Access is
      Result : constant Package_Body_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Package_Body_Declarations
            .Implicit_Package_Body_Declaration'
            (Program.Nodes.Package_Body_Declarations.Create
               (Name => Name, Aspects => Aspects, Declarations => Declarations,
                Statements => Statements,
                Exception_Handlers => Exception_Handlers, End_Name => End_Name,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Package_Body_Declarations
        .Package_Body_Declaration_Access
        (Result);
   end Create_Package_Body_Declaration;

   function Create_Object_Renaming_Declaration
    (Self : Element_Factory;
     Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Object_Subtype       : not null Program.Elements.Element_Access;
     Renamed_Object       : not null Program.Elements.Expressions
         .Expression_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Has_Not_Null         : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Object_Renaming_Declarations
          .Object_Renaming_Declaration_Access is
      Result : constant Object_Renaming_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Object_Renaming_Declarations
            .Implicit_Object_Renaming_Declaration'
            (Program.Nodes.Object_Renaming_Declarations.Create
               (Names => Names, Object_Subtype => Object_Subtype,
                Renamed_Object => Renamed_Object, Aspects => Aspects,
                Has_Not_Null => Has_Not_Null,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Object_Renaming_Declarations
        .Object_Renaming_Declaration_Access
        (Result);
   end Create_Object_Renaming_Declaration;

   function Create_Exception_Renaming_Declaration
    (Self : Element_Factory;
     Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Renamed_Exception    : not null Program.Elements.Expressions
         .Expression_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Exception_Renaming_Declarations
          .Exception_Renaming_Declaration_Access is
      Result : constant Exception_Renaming_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Exception_Renaming_Declarations
            .Implicit_Exception_Renaming_Declaration'
            (Program.Nodes.Exception_Renaming_Declarations.Create
               (Names => Names, Renamed_Exception => Renamed_Exception,
                Aspects => Aspects, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Exception_Renaming_Declarations
        .Exception_Renaming_Declaration_Access
        (Result);
   end Create_Exception_Renaming_Declaration;

   function Create_Procedure_Renaming_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Parameters           : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Renamed_Procedure    : Program.Elements.Expressions.Expression_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Has_Not              : Boolean := False;
     Has_Overriding       : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Procedure_Renaming_Declarations
          .Procedure_Renaming_Declaration_Access is
      Result : constant Procedure_Renaming_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Procedure_Renaming_Declarations
            .Implicit_Procedure_Renaming_Declaration'
            (Program.Nodes.Procedure_Renaming_Declarations.Create
               (Name => Name, Parameters => Parameters,
                Renamed_Procedure => Renamed_Procedure, Aspects => Aspects,
                Has_Not => Has_Not, Has_Overriding => Has_Overriding,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Procedure_Renaming_Declarations
        .Procedure_Renaming_Declaration_Access
        (Result);
   end Create_Procedure_Renaming_Declaration;

   function Create_Function_Renaming_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Parameters           : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Result_Subtype       : not null Program.Elements.Element_Access;
     Renamed_Function     : Program.Elements.Expressions.Expression_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Has_Not              : Boolean := False;
     Has_Overriding       : Boolean := False;
     Has_Not_Null         : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Function_Renaming_Declarations
          .Function_Renaming_Declaration_Access is
      Result : constant Function_Renaming_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Function_Renaming_Declarations
            .Implicit_Function_Renaming_Declaration'
            (Program.Nodes.Function_Renaming_Declarations.Create
               (Name => Name, Parameters => Parameters,
                Result_Subtype => Result_Subtype,
                Renamed_Function => Renamed_Function, Aspects => Aspects,
                Has_Not => Has_Not, Has_Overriding => Has_Overriding,
                Has_Not_Null => Has_Not_Null,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Function_Renaming_Declarations
        .Function_Renaming_Declaration_Access
        (Result);
   end Create_Function_Renaming_Declaration;

   function Create_Package_Renaming_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Renamed_Package      : not null Program.Elements.Expressions
         .Expression_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Package_Renaming_Declarations
          .Package_Renaming_Declaration_Access is
      Result : constant Package_Renaming_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Package_Renaming_Declarations
            .Implicit_Package_Renaming_Declaration'
            (Program.Nodes.Package_Renaming_Declarations.Create
               (Name => Name, Renamed_Package => Renamed_Package,
                Aspects => Aspects, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Package_Renaming_Declarations
        .Package_Renaming_Declaration_Access
        (Result);
   end Create_Package_Renaming_Declaration;

   function Create_Generic_Package_Renaming_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Renamed_Package      : not null Program.Elements.Expressions
         .Expression_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Generic_Package_Renaming_Declarations
          .Generic_Package_Renaming_Declaration_Access is
      Result : constant Generic_Package_Renaming_Declaration_Access :=

          new (Self.Subpool) Program.Nodes
            .Generic_Package_Renaming_Declarations
            .Implicit_Generic_Package_Renaming_Declaration'
            (Program.Nodes.Generic_Package_Renaming_Declarations.Create
               (Name => Name, Renamed_Package => Renamed_Package,
                Aspects => Aspects, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Generic_Package_Renaming_Declarations
        .Generic_Package_Renaming_Declaration_Access
        (Result);
   end Create_Generic_Package_Renaming_Declaration;

   function Create_Generic_Procedure_Renaming_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Renamed_Procedure    : not null Program.Elements.Expressions
         .Expression_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Generic_Procedure_Renaming_Declarations
          .Generic_Procedure_Renaming_Declaration_Access is
      Result : constant Generic_Procedure_Renaming_Declaration_Access :=

          new (Self.Subpool) Program.Nodes
            .Generic_Procedure_Renaming_Declarations
            .Implicit_Generic_Procedure_Renaming_Declaration'
            (Program.Nodes.Generic_Procedure_Renaming_Declarations.Create
               (Name => Name, Renamed_Procedure => Renamed_Procedure,
                Aspects => Aspects, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Generic_Procedure_Renaming_Declarations
        .Generic_Procedure_Renaming_Declaration_Access
        (Result);
   end Create_Generic_Procedure_Renaming_Declaration;

   function Create_Generic_Function_Renaming_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Renamed_Function     : not null Program.Elements.Expressions
         .Expression_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Generic_Function_Renaming_Declarations
          .Generic_Function_Renaming_Declaration_Access is
      Result : constant Generic_Function_Renaming_Declaration_Access :=

          new (Self.Subpool) Program.Nodes
            .Generic_Function_Renaming_Declarations
            .Implicit_Generic_Function_Renaming_Declaration'
            (Program.Nodes.Generic_Function_Renaming_Declarations.Create
               (Name => Name, Renamed_Function => Renamed_Function,
                Aspects => Aspects, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Generic_Function_Renaming_Declarations
        .Generic_Function_Renaming_Declaration_Access
        (Result);
   end Create_Generic_Function_Renaming_Declaration;

   function Create_Task_Body_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Declarations         : not null Program.Element_Vectors
         .Element_Vector_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Handlers   : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Task_Body_Declarations
          .Task_Body_Declaration_Access is
      Result : constant Task_Body_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Task_Body_Declarations
            .Implicit_Task_Body_Declaration'
            (Program.Nodes.Task_Body_Declarations.Create
               (Name => Name, Aspects => Aspects, Declarations => Declarations,
                Statements => Statements,
                Exception_Handlers => Exception_Handlers, End_Name => End_Name,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Task_Body_Declarations
        .Task_Body_Declaration_Access
        (Result);
   end Create_Task_Body_Declaration;

   function Create_Protected_Body_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Protected_Operations : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Protected_Body_Declarations
          .Protected_Body_Declaration_Access is
      Result : constant Protected_Body_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Protected_Body_Declarations
            .Implicit_Protected_Body_Declaration'
            (Program.Nodes.Protected_Body_Declarations.Create
               (Name => Name, Aspects => Aspects,
                Protected_Operations => Protected_Operations,
                End_Name => End_Name,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Protected_Body_Declarations
        .Protected_Body_Declaration_Access
        (Result);
   end Create_Protected_Body_Declaration;

   function Create_Entry_Declaration
    (Self : Element_Factory;
     Name                    : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Entry_Family_Definition : Program.Elements.Discrete_Subtype_Definitions
         .Discrete_Subtype_Definition_Access;
     Parameters              : not null Program.Elements
         .Parameter_Specifications.Parameter_Specification_Vector_Access;
     Aspects                 : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Has_Not                 : Boolean := False;
     Has_Overriding          : Boolean := False;
     Is_Part_Of_Implicit     : Boolean := False;
     Is_Part_Of_Inherited    : Boolean := False;
     Is_Part_Of_Instance     : Boolean := False)
      return not null Program.Elements.Entry_Declarations
          .Entry_Declaration_Access is
      Result : constant Entry_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Entry_Declarations
            .Implicit_Entry_Declaration'
            (Program.Nodes.Entry_Declarations.Create
               (Name => Name,
                Entry_Family_Definition => Entry_Family_Definition,
                Parameters => Parameters, Aspects => Aspects,
                Has_Not => Has_Not, Has_Overriding => Has_Overriding,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Entry_Declarations.Entry_Declaration_Access
        (Result);
   end Create_Entry_Declaration;

   function Create_Entry_Body_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Entry_Index          : not null Program.Elements
         .Entry_Index_Specifications.Entry_Index_Specification_Access;
     Parameters           : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Entry_Barrier        : not null Program.Elements.Expressions
         .Expression_Access;
     Declarations         : not null Program.Element_Vectors
         .Element_Vector_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Handlers   : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Entry_Body_Declarations
          .Entry_Body_Declaration_Access is
      Result : constant Entry_Body_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Entry_Body_Declarations
            .Implicit_Entry_Body_Declaration'
            (Program.Nodes.Entry_Body_Declarations.Create
               (Name => Name, Entry_Index => Entry_Index,
                Parameters => Parameters, Entry_Barrier => Entry_Barrier,
                Declarations => Declarations, Statements => Statements,
                Exception_Handlers => Exception_Handlers, End_Name => End_Name,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Entry_Body_Declarations
        .Entry_Body_Declaration_Access
        (Result);
   end Create_Entry_Body_Declaration;

   function Create_Entry_Index_Specification
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Entry_Index_Subtype  : not null Program.Elements
         .Discrete_Subtype_Definitions.Discrete_Subtype_Definition_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Entry_Index_Specifications
          .Entry_Index_Specification_Access is
      Result : constant Entry_Index_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Entry_Index_Specifications
            .Implicit_Entry_Index_Specification'
            (Program.Nodes.Entry_Index_Specifications.Create
               (Name => Name, Entry_Index_Subtype => Entry_Index_Subtype,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Entry_Index_Specifications
        .Entry_Index_Specification_Access
        (Result);
   end Create_Entry_Index_Specification;

   function Create_Procedure_Body_Stub
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Parameters           : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Has_Not              : Boolean := False;
     Has_Overriding       : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Procedure_Body_Stubs
          .Procedure_Body_Stub_Access is
      Result : constant Procedure_Body_Stub_Access :=

          new (Self.Subpool) Program.Nodes.Procedure_Body_Stubs
            .Implicit_Procedure_Body_Stub'
            (Program.Nodes.Procedure_Body_Stubs.Create
               (Name => Name, Parameters => Parameters, Aspects => Aspects,
                Has_Not => Has_Not, Has_Overriding => Has_Overriding,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Procedure_Body_Stubs.Procedure_Body_Stub_Access
        (Result);
   end Create_Procedure_Body_Stub;

   function Create_Function_Body_Stub
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Parameters           : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Result_Subtype       : not null Program.Elements.Element_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Has_Not              : Boolean := False;
     Has_Overriding       : Boolean := False;
     Has_Not_Null         : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Function_Body_Stubs
          .Function_Body_Stub_Access is
      Result : constant Function_Body_Stub_Access :=

          new (Self.Subpool) Program.Nodes.Function_Body_Stubs
            .Implicit_Function_Body_Stub'
            (Program.Nodes.Function_Body_Stubs.Create
               (Name => Name, Parameters => Parameters,
                Result_Subtype => Result_Subtype, Aspects => Aspects,
                Has_Not => Has_Not, Has_Overriding => Has_Overriding,
                Has_Not_Null => Has_Not_Null,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Function_Body_Stubs.Function_Body_Stub_Access
        (Result);
   end Create_Function_Body_Stub;

   function Create_Package_Body_Stub
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Package_Body_Stubs
          .Package_Body_Stub_Access is
      Result : constant Package_Body_Stub_Access :=

          new (Self.Subpool) Program.Nodes.Package_Body_Stubs
            .Implicit_Package_Body_Stub'
            (Program.Nodes.Package_Body_Stubs.Create
               (Name => Name, Aspects => Aspects,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Package_Body_Stubs.Package_Body_Stub_Access
        (Result);
   end Create_Package_Body_Stub;

   function Create_Task_Body_Stub
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Task_Body_Stubs.Task_Body_Stub_Access is
      Result : constant Task_Body_Stub_Access :=

          new (Self.Subpool) Program.Nodes.Task_Body_Stubs
            .Implicit_Task_Body_Stub'
            (Program.Nodes.Task_Body_Stubs.Create
               (Name => Name, Aspects => Aspects,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Task_Body_Stubs.Task_Body_Stub_Access (Result);
   end Create_Task_Body_Stub;

   function Create_Protected_Body_Stub
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Protected_Body_Stubs
          .Protected_Body_Stub_Access is
      Result : constant Protected_Body_Stub_Access :=

          new (Self.Subpool) Program.Nodes.Protected_Body_Stubs
            .Implicit_Protected_Body_Stub'
            (Program.Nodes.Protected_Body_Stubs.Create
               (Name => Name, Aspects => Aspects,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Protected_Body_Stubs.Protected_Body_Stub_Access
        (Result);
   end Create_Protected_Body_Stub;

   function Create_Exception_Declaration
    (Self : Element_Factory;
     Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Exception_Declarations
          .Exception_Declaration_Access is
      Result : constant Exception_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Exception_Declarations
            .Implicit_Exception_Declaration'
            (Program.Nodes.Exception_Declarations.Create
               (Names => Names, Aspects => Aspects,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Exception_Declarations
        .Exception_Declaration_Access
        (Result);
   end Create_Exception_Declaration;

   function Create_Choice_Parameter_Specification
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Choice_Parameter_Specifications
          .Choice_Parameter_Specification_Access is
      Result : constant Choice_Parameter_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Choice_Parameter_Specifications
            .Implicit_Choice_Parameter_Specification'
            (Program.Nodes.Choice_Parameter_Specifications.Create
               (Name => Name, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Choice_Parameter_Specifications
        .Choice_Parameter_Specification_Access
        (Result);
   end Create_Choice_Parameter_Specification;

   function Create_Generic_Package_Declaration
    (Self : Element_Factory;
     Formal_Parameters    : not null Program.Element_Vectors
         .Element_Vector_Access;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Visible_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     Private_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Name             : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Generic_Package_Declarations
          .Generic_Package_Declaration_Access is
      Result : constant Generic_Package_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Generic_Package_Declarations
            .Implicit_Generic_Package_Declaration'
            (Program.Nodes.Generic_Package_Declarations.Create
               (Formal_Parameters => Formal_Parameters, Name => Name,
                Aspects => Aspects,
                Visible_Declarations => Visible_Declarations,
                Private_Declarations => Private_Declarations,
                End_Name => End_Name,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Generic_Package_Declarations
        .Generic_Package_Declaration_Access
        (Result);
   end Create_Generic_Package_Declaration;

   function Create_Generic_Procedure_Declaration
    (Self : Element_Factory;
     Formal_Parameters    : not null Program.Element_Vectors
         .Element_Vector_Access;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Parameters           : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Generic_Procedure_Declarations
          .Generic_Procedure_Declaration_Access is
      Result : constant Generic_Procedure_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Generic_Procedure_Declarations
            .Implicit_Generic_Procedure_Declaration'
            (Program.Nodes.Generic_Procedure_Declarations.Create
               (Formal_Parameters => Formal_Parameters, Name => Name,
                Parameters => Parameters, Aspects => Aspects,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Generic_Procedure_Declarations
        .Generic_Procedure_Declaration_Access
        (Result);
   end Create_Generic_Procedure_Declaration;

   function Create_Generic_Function_Declaration
    (Self : Element_Factory;
     Formal_Parameters    : not null Program.Element_Vectors
         .Element_Vector_Access;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Parameters           : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Result_Subtype       : not null Program.Elements.Element_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Has_Not_Null         : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Generic_Function_Declarations
          .Generic_Function_Declaration_Access is
      Result : constant Generic_Function_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Generic_Function_Declarations
            .Implicit_Generic_Function_Declaration'
            (Program.Nodes.Generic_Function_Declarations.Create
               (Formal_Parameters => Formal_Parameters, Name => Name,
                Parameters => Parameters, Result_Subtype => Result_Subtype,
                Aspects => Aspects, Has_Not_Null => Has_Not_Null,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Generic_Function_Declarations
        .Generic_Function_Declaration_Access
        (Result);
   end Create_Generic_Function_Declaration;

   function Create_Package_Instantiation
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Generic_Package_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Parameters           : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Package_Instantiations
          .Package_Instantiation_Access is
      Result : constant Package_Instantiation_Access :=

          new (Self.Subpool) Program.Nodes.Package_Instantiations
            .Implicit_Package_Instantiation'
            (Program.Nodes.Package_Instantiations.Create
               (Name => Name, Generic_Package_Name => Generic_Package_Name,
                Parameters => Parameters, Aspects => Aspects,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Package_Instantiations
        .Package_Instantiation_Access
        (Result);
   end Create_Package_Instantiation;

   function Create_Procedure_Instantiation
    (Self : Element_Factory;
     Name                   : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Generic_Procedure_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Parameters             : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Aspects                : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Has_Not                : Boolean := False;
     Has_Overriding         : Boolean := False;
     Is_Part_Of_Implicit    : Boolean := False;
     Is_Part_Of_Inherited   : Boolean := False;
     Is_Part_Of_Instance    : Boolean := False)
      return not null Program.Elements.Procedure_Instantiations
          .Procedure_Instantiation_Access is
      Result : constant Procedure_Instantiation_Access :=

          new (Self.Subpool) Program.Nodes.Procedure_Instantiations
            .Implicit_Procedure_Instantiation'
            (Program.Nodes.Procedure_Instantiations.Create
               (Name => Name, Generic_Procedure_Name => Generic_Procedure_Name,
                Parameters => Parameters, Aspects => Aspects,
                Has_Not => Has_Not, Has_Overriding => Has_Overriding,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Procedure_Instantiations
        .Procedure_Instantiation_Access
        (Result);
   end Create_Procedure_Instantiation;

   function Create_Function_Instantiation
    (Self : Element_Factory;
     Name                  : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Generic_Function_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Parameters            : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Aspects               : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Has_Not               : Boolean := False;
     Has_Overriding        : Boolean := False;
     Is_Part_Of_Implicit   : Boolean := False;
     Is_Part_Of_Inherited  : Boolean := False;
     Is_Part_Of_Instance   : Boolean := False)
      return not null Program.Elements.Function_Instantiations
          .Function_Instantiation_Access is
      Result : constant Function_Instantiation_Access :=

          new (Self.Subpool) Program.Nodes.Function_Instantiations
            .Implicit_Function_Instantiation'
            (Program.Nodes.Function_Instantiations.Create
               (Name => Name, Generic_Function_Name => Generic_Function_Name,
                Parameters => Parameters, Aspects => Aspects,
                Has_Not => Has_Not, Has_Overriding => Has_Overriding,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Function_Instantiations
        .Function_Instantiation_Access
        (Result);
   end Create_Function_Instantiation;

   function Create_Formal_Object_Declaration
    (Self : Element_Factory;
     Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Object_Subtype       : not null Program.Elements.Element_Access;
     Default_Expression   : Program.Elements.Expressions.Expression_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Has_In               : Boolean := False;
     Has_Out              : Boolean := False;
     Has_Not_Null         : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Formal_Object_Declarations
          .Formal_Object_Declaration_Access is
      Result : constant Formal_Object_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Object_Declarations
            .Implicit_Formal_Object_Declaration'
            (Program.Nodes.Formal_Object_Declarations.Create
               (Names => Names, Object_Subtype => Object_Subtype,
                Default_Expression => Default_Expression, Aspects => Aspects,
                Has_In => Has_In, Has_Out => Has_Out,
                Has_Not_Null => Has_Not_Null,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Formal_Object_Declarations
        .Formal_Object_Declaration_Access
        (Result);
   end Create_Formal_Object_Declaration;

   function Create_Formal_Type_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Discriminant_Part    : Program.Elements.Definitions.Definition_Access;
     Definition           : not null Program.Elements.Formal_Type_Definitions
         .Formal_Type_Definition_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Formal_Type_Declarations
          .Formal_Type_Declaration_Access is
      Result : constant Formal_Type_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Type_Declarations
            .Implicit_Formal_Type_Declaration'
            (Program.Nodes.Formal_Type_Declarations.Create
               (Name => Name, Discriminant_Part => Discriminant_Part,
                Definition => Definition, Aspects => Aspects,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Formal_Type_Declarations
        .Formal_Type_Declaration_Access
        (Result);
   end Create_Formal_Type_Declaration;

   function Create_Formal_Procedure_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Parameters           : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Subprogram_Default   : Program.Elements.Expressions.Expression_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Has_Abstract         : Boolean := False;
     Has_Null             : Boolean := False;
     Has_Box              : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Formal_Procedure_Declarations
          .Formal_Procedure_Declaration_Access is
      Result : constant Formal_Procedure_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Procedure_Declarations
            .Implicit_Formal_Procedure_Declaration'
            (Program.Nodes.Formal_Procedure_Declarations.Create
               (Name => Name, Parameters => Parameters,
                Subprogram_Default => Subprogram_Default, Aspects => Aspects,
                Has_Abstract => Has_Abstract, Has_Null => Has_Null,
                Has_Box => Has_Box, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Formal_Procedure_Declarations
        .Formal_Procedure_Declaration_Access
        (Result);
   end Create_Formal_Procedure_Declaration;

   function Create_Formal_Function_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Parameters           : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Result_Subtype       : not null Program.Elements.Element_Access;
     Subprogram_Default   : Program.Elements.Expressions.Expression_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Has_Not_Null         : Boolean := False;
     Has_Abstract         : Boolean := False;
     Has_Box              : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Formal_Function_Declarations
          .Formal_Function_Declaration_Access is
      Result : constant Formal_Function_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Function_Declarations
            .Implicit_Formal_Function_Declaration'
            (Program.Nodes.Formal_Function_Declarations.Create
               (Name => Name, Parameters => Parameters,
                Result_Subtype => Result_Subtype,
                Subprogram_Default => Subprogram_Default, Aspects => Aspects,
                Has_Not_Null => Has_Not_Null, Has_Abstract => Has_Abstract,
                Has_Box => Has_Box, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Formal_Function_Declarations
        .Formal_Function_Declaration_Access
        (Result);
   end Create_Formal_Function_Declaration;

   function Create_Formal_Package_Declaration
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Generic_Package_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Parameters           : not null Program.Elements
         .Formal_Package_Associations.Formal_Package_Association_Vector_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Formal_Package_Declarations
          .Formal_Package_Declaration_Access is
      Result : constant Formal_Package_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Package_Declarations
            .Implicit_Formal_Package_Declaration'
            (Program.Nodes.Formal_Package_Declarations.Create
               (Name => Name, Generic_Package_Name => Generic_Package_Name,
                Parameters => Parameters, Aspects => Aspects,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Formal_Package_Declarations
        .Formal_Package_Declaration_Access
        (Result);
   end Create_Formal_Package_Declaration;

   function Create_Subtype_Indication
    (Self : Element_Factory;
     Subtype_Mark         : not null Program.Elements.Expressions
         .Expression_Access;
     Constraint           : Program.Elements.Constraints.Constraint_Access;
     Has_Not_Null         : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access is
      Result : constant Subtype_Indication_Access :=

          new (Self.Subpool) Program.Nodes.Subtype_Indications
            .Implicit_Subtype_Indication'
            (Program.Nodes.Subtype_Indications.Create
               (Subtype_Mark => Subtype_Mark, Constraint => Constraint,
                Has_Not_Null => Has_Not_Null,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Subtype_Indications.Subtype_Indication_Access
        (Result);
   end Create_Subtype_Indication;

   function Create_Component_Definition
    (Self : Element_Factory;
     Subtype_Indication   : not null Program.Elements.Element_Access;
     Has_Aliased          : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Component_Definitions
          .Component_Definition_Access is
      Result : constant Component_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Component_Definitions
            .Implicit_Component_Definition'
            (Program.Nodes.Component_Definitions.Create
               (Subtype_Indication => Subtype_Indication,
                Has_Aliased => Has_Aliased,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Component_Definitions.Component_Definition_Access
        (Result);
   end Create_Component_Definition;

   function Create_Unknown_Discriminant_Part
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Unknown_Discriminant_Parts
          .Unknown_Discriminant_Part_Access is
      Result : constant Unknown_Discriminant_Part_Access :=

          new (Self.Subpool) Program.Nodes.Unknown_Discriminant_Parts
            .Implicit_Unknown_Discriminant_Part'
            (Program.Nodes.Unknown_Discriminant_Parts.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Unknown_Discriminant_Parts
        .Unknown_Discriminant_Part_Access
        (Result);
   end Create_Unknown_Discriminant_Part;

   function Create_Known_Discriminant_Part
    (Self : Element_Factory;
     Discriminants        : not null Program.Elements
         .Discriminant_Specifications.Discriminant_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Known_Discriminant_Parts
          .Known_Discriminant_Part_Access is
      Result : constant Known_Discriminant_Part_Access :=

          new (Self.Subpool) Program.Nodes.Known_Discriminant_Parts
            .Implicit_Known_Discriminant_Part'
            (Program.Nodes.Known_Discriminant_Parts.Create
               (Discriminants => Discriminants,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Known_Discriminant_Parts
        .Known_Discriminant_Part_Access
        (Result);
   end Create_Known_Discriminant_Part;

   function Create_Record_Definition
    (Self : Element_Factory;
     Components           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Record_Definitions
          .Record_Definition_Access is
      Result : constant Record_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Record_Definitions
            .Implicit_Record_Definition'
            (Program.Nodes.Record_Definitions.Create
               (Components => Components,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Record_Definitions.Record_Definition_Access
        (Result);
   end Create_Record_Definition;

   function Create_Null_Component
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Null_Components.Null_Component_Access is
      Result : constant Null_Component_Access :=

          new (Self.Subpool) Program.Nodes.Null_Components
            .Implicit_Null_Component'
            (Program.Nodes.Null_Components.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Null_Components.Null_Component_Access (Result);
   end Create_Null_Component;

   function Create_Variant_Part
    (Self : Element_Factory;
     Discriminant         : not null Program.Elements.Identifiers
         .Identifier_Access;
     Variants             : not null Program.Elements.Variants
         .Variant_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Variant_Parts.Variant_Part_Access is
      Result : constant Variant_Part_Access :=

          new (Self.Subpool) Program.Nodes.Variant_Parts.Implicit_Variant_Part'
            (Program.Nodes.Variant_Parts.Create
               (Discriminant => Discriminant, Variants => Variants,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Variant_Parts.Variant_Part_Access (Result);
   end Create_Variant_Part;

   function Create_Variant
    (Self : Element_Factory;
     Choices              : not null Program.Element_Vectors
         .Element_Vector_Access;
     Components           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Variants.Variant_Access is
      Result : constant Variant_Access :=

          new (Self.Subpool) Program.Nodes.Variants.Implicit_Variant'
            (Program.Nodes.Variants.Create
               (Choices => Choices, Components => Components,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Variants.Variant_Access (Result);
   end Create_Variant;

   function Create_Others_Choice
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Others_Choices.Others_Choice_Access is
      Result : constant Others_Choice_Access :=

          new (Self.Subpool) Program.Nodes.Others_Choices
            .Implicit_Others_Choice'
            (Program.Nodes.Others_Choices.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Others_Choices.Others_Choice_Access (Result);
   end Create_Others_Choice;

   function Create_Private_Type_Definition
    (Self : Element_Factory;
     Has_Abstract         : Boolean := False;
     Has_Tagged           : Boolean := False;
     Has_Limited          : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Private_Type_Definitions
          .Private_Type_Definition_Access is
      Result : constant Private_Type_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Private_Type_Definitions
            .Implicit_Private_Type_Definition'
            (Program.Nodes.Private_Type_Definitions.Create
               (Has_Abstract => Has_Abstract, Has_Tagged => Has_Tagged,
                Has_Limited => Has_Limited,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Private_Type_Definitions
        .Private_Type_Definition_Access
        (Result);
   end Create_Private_Type_Definition;

   function Create_Private_Extension_Definition
    (Self : Element_Factory;
     Ancestor             : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Progenitors          : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Has_Abstract         : Boolean := False;
     Has_Limited          : Boolean := False;
     Has_Synchronized     : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Private_Extension_Definitions
          .Private_Extension_Definition_Access is
      Result : constant Private_Extension_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Private_Extension_Definitions
            .Implicit_Private_Extension_Definition'
            (Program.Nodes.Private_Extension_Definitions.Create
               (Ancestor => Ancestor, Progenitors => Progenitors,
                Has_Abstract => Has_Abstract, Has_Limited => Has_Limited,
                Has_Synchronized => Has_Synchronized,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Private_Extension_Definitions
        .Private_Extension_Definition_Access
        (Result);
   end Create_Private_Extension_Definition;

   function Create_Incomplete_Type_Definition
    (Self : Element_Factory;
     Has_Tagged           : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Incomplete_Type_Definitions
          .Incomplete_Type_Definition_Access is
      Result : constant Incomplete_Type_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Incomplete_Type_Definitions
            .Implicit_Incomplete_Type_Definition'
            (Program.Nodes.Incomplete_Type_Definitions.Create
               (Has_Tagged => Has_Tagged,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Incomplete_Type_Definitions
        .Incomplete_Type_Definition_Access
        (Result);
   end Create_Incomplete_Type_Definition;

   function Create_Task_Definition
    (Self : Element_Factory;
     Visible_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     Private_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Task_Definitions
          .Task_Definition_Access is
      Result : constant Task_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Task_Definitions
            .Implicit_Task_Definition'
            (Program.Nodes.Task_Definitions.Create
               (Visible_Declarations => Visible_Declarations,
                Private_Declarations => Private_Declarations,
                End_Name => End_Name,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Task_Definitions.Task_Definition_Access (Result);
   end Create_Task_Definition;

   function Create_Protected_Definition
    (Self : Element_Factory;
     Visible_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     Private_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Protected_Definitions
          .Protected_Definition_Access is
      Result : constant Protected_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Protected_Definitions
            .Implicit_Protected_Definition'
            (Program.Nodes.Protected_Definitions.Create
               (Visible_Declarations => Visible_Declarations,
                Private_Declarations => Private_Declarations,
                End_Name => End_Name,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Protected_Definitions.Protected_Definition_Access
        (Result);
   end Create_Protected_Definition;

   function Create_Aspect_Specification
    (Self : Element_Factory;
     Aspect_Mark          : not null Program.Elements.Expressions
         .Expression_Access;
     Aspect_Definition    : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Access is
      Result : constant Aspect_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Aspect_Specifications
            .Implicit_Aspect_Specification'
            (Program.Nodes.Aspect_Specifications.Create
               (Aspect_Mark => Aspect_Mark,
                Aspect_Definition => Aspect_Definition,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Aspect_Specifications.Aspect_Specification_Access
        (Result);
   end Create_Aspect_Specification;

   function Create_Real_Range_Specification
    (Self : Element_Factory;
     Lower_Bound          : not null Program.Elements.Expressions
         .Expression_Access;
     Upper_Bound          : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Access is
      Result : constant Real_Range_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Real_Range_Specifications
            .Implicit_Real_Range_Specification'
            (Program.Nodes.Real_Range_Specifications.Create
               (Lower_Bound => Lower_Bound, Upper_Bound => Upper_Bound,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Real_Range_Specifications
        .Real_Range_Specification_Access
        (Result);
   end Create_Real_Range_Specification;

   function Create_Numeric_Literal
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Numeric_Literals
          .Numeric_Literal_Access is
      Result : constant Numeric_Literal_Access :=

          new (Self.Subpool) Program.Nodes.Numeric_Literals
            .Implicit_Numeric_Literal'
            (Program.Nodes.Numeric_Literals.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Numeric_Literals.Numeric_Literal_Access (Result);
   end Create_Numeric_Literal;

   function Create_String_Literal
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.String_Literals.String_Literal_Access is
      Result : constant String_Literal_Access :=

          new (Self.Subpool) Program.Nodes.String_Literals
            .Implicit_String_Literal'
            (Program.Nodes.String_Literals.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.String_Literals.String_Literal_Access (Result);
   end Create_String_Literal;

   function Create_Identifier
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Identifiers.Identifier_Access is
      Result : constant Identifier_Access :=

          new (Self.Subpool) Program.Nodes.Identifiers.Implicit_Identifier'
            (Program.Nodes.Identifiers.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Identifiers.Identifier_Access (Result);
   end Create_Identifier;

   function Create_Operator_Symbol
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Operator_Symbols
          .Operator_Symbol_Access is
      Result : constant Operator_Symbol_Access :=

          new (Self.Subpool) Program.Nodes.Operator_Symbols
            .Implicit_Operator_Symbol'
            (Program.Nodes.Operator_Symbols.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Operator_Symbols.Operator_Symbol_Access (Result);
   end Create_Operator_Symbol;

   function Create_Character_Literal
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Character_Literals
          .Character_Literal_Access is
      Result : constant Character_Literal_Access :=

          new (Self.Subpool) Program.Nodes.Character_Literals
            .Implicit_Character_Literal'
            (Program.Nodes.Character_Literals.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Character_Literals.Character_Literal_Access
        (Result);
   end Create_Character_Literal;

   function Create_Explicit_Dereference
    (Self : Element_Factory;
     Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Explicit_Dereferences
          .Explicit_Dereference_Access is
      Result : constant Explicit_Dereference_Access :=

          new (Self.Subpool) Program.Nodes.Explicit_Dereferences
            .Implicit_Explicit_Dereference'
            (Program.Nodes.Explicit_Dereferences.Create
               (Prefix => Prefix, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Explicit_Dereferences.Explicit_Dereference_Access
        (Result);
   end Create_Explicit_Dereference;

   function Create_Function_Call
    (Self : Element_Factory;
     Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Parameters           : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Function_Calls.Function_Call_Access is
      Result : constant Function_Call_Access :=

          new (Self.Subpool) Program.Nodes.Function_Calls
            .Implicit_Function_Call'
            (Program.Nodes.Function_Calls.Create
               (Prefix => Prefix, Parameters => Parameters,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Function_Calls.Function_Call_Access (Result);
   end Create_Function_Call;

   function Create_Indexed_Component
    (Self : Element_Factory;
     Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Expressions          : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Indexed_Components
          .Indexed_Component_Access is
      Result : constant Indexed_Component_Access :=

          new (Self.Subpool) Program.Nodes.Indexed_Components
            .Implicit_Indexed_Component'
            (Program.Nodes.Indexed_Components.Create
               (Prefix => Prefix, Expressions => Expressions,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Indexed_Components.Indexed_Component_Access
        (Result);
   end Create_Indexed_Component;

   function Create_Slice
    (Self : Element_Factory;
     Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Slice_Range          : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Slices.Slice_Access is
      Result : constant Slice_Access :=

          new (Self.Subpool) Program.Nodes.Slices.Implicit_Slice'
            (Program.Nodes.Slices.Create
               (Prefix => Prefix, Slice_Range => Slice_Range,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Slices.Slice_Access (Result);
   end Create_Slice;

   function Create_Selected_Component
    (Self : Element_Factory;
     Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Selector             : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Selected_Components
          .Selected_Component_Access is
      Result : constant Selected_Component_Access :=

          new (Self.Subpool) Program.Nodes.Selected_Components
            .Implicit_Selected_Component'
            (Program.Nodes.Selected_Components.Create
               (Prefix => Prefix, Selector => Selector,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Selected_Components.Selected_Component_Access
        (Result);
   end Create_Selected_Component;

   function Create_Attribute_Reference
    (Self : Element_Factory;
     Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Attribute_Designator : not null Program.Elements.Identifiers
         .Identifier_Access;
     Expressions          : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Attribute_References
          .Attribute_Reference_Access is
      Result : constant Attribute_Reference_Access :=

          new (Self.Subpool) Program.Nodes.Attribute_References
            .Implicit_Attribute_Reference'
            (Program.Nodes.Attribute_References.Create
               (Prefix => Prefix, Attribute_Designator => Attribute_Designator,
                Expressions => Expressions,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Attribute_References.Attribute_Reference_Access
        (Result);
   end Create_Attribute_Reference;

   function Create_Record_Aggregate
    (Self : Element_Factory;
     Components           : not null Program.Elements
         .Record_Component_Associations
         .Record_Component_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Record_Aggregates
          .Record_Aggregate_Access is
      Result : constant Record_Aggregate_Access :=

          new (Self.Subpool) Program.Nodes.Record_Aggregates
            .Implicit_Record_Aggregate'
            (Program.Nodes.Record_Aggregates.Create
               (Components => Components,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Record_Aggregates.Record_Aggregate_Access
        (Result);
   end Create_Record_Aggregate;

   function Create_Extension_Aggregate
    (Self : Element_Factory;
     Ancestor             : not null Program.Elements.Expressions
         .Expression_Access;
     Components           : not null Program.Elements
         .Record_Component_Associations
         .Record_Component_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Extension_Aggregates
          .Extension_Aggregate_Access is
      Result : constant Extension_Aggregate_Access :=

          new (Self.Subpool) Program.Nodes.Extension_Aggregates
            .Implicit_Extension_Aggregate'
            (Program.Nodes.Extension_Aggregates.Create
               (Ancestor => Ancestor, Components => Components,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Extension_Aggregates.Extension_Aggregate_Access
        (Result);
   end Create_Extension_Aggregate;

   function Create_Array_Aggregate
    (Self : Element_Factory;
     Components           : not null Program.Elements
         .Array_Component_Associations
         .Array_Component_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Array_Aggregates
          .Array_Aggregate_Access is
      Result : constant Array_Aggregate_Access :=

          new (Self.Subpool) Program.Nodes.Array_Aggregates
            .Implicit_Array_Aggregate'
            (Program.Nodes.Array_Aggregates.Create
               (Components => Components,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Array_Aggregates.Array_Aggregate_Access (Result);
   end Create_Array_Aggregate;

   function Create_Short_Circuit_Operation
    (Self : Element_Factory;
     Left                 : not null Program.Elements.Expressions
         .Expression_Access;
     Right                : not null Program.Elements.Expressions
         .Expression_Access;
     Has_And_Then         : Boolean := False;
     Has_Or_Else          : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Short_Circuit_Operations
          .Short_Circuit_Operation_Access is
      Result : constant Short_Circuit_Operation_Access :=

          new (Self.Subpool) Program.Nodes.Short_Circuit_Operations
            .Implicit_Short_Circuit_Operation'
            (Program.Nodes.Short_Circuit_Operations.Create
               (Left => Left, Right => Right, Has_And_Then => Has_And_Then,
                Has_Or_Else => Has_Or_Else,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Short_Circuit_Operations
        .Short_Circuit_Operation_Access
        (Result);
   end Create_Short_Circuit_Operation;

   function Create_Membership_Test
    (Self : Element_Factory;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Choices              : not null Program.Element_Vectors
         .Element_Vector_Access;
     Has_Not              : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Membership_Tests
          .Membership_Test_Access is
      Result : constant Membership_Test_Access :=

          new (Self.Subpool) Program.Nodes.Membership_Tests
            .Implicit_Membership_Test'
            (Program.Nodes.Membership_Tests.Create
               (Expression => Expression, Choices => Choices,
                Has_Not => Has_Not, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Membership_Tests.Membership_Test_Access (Result);
   end Create_Membership_Test;

   function Create_Null_Literal
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Null_Literals.Null_Literal_Access is
      Result : constant Null_Literal_Access :=

          new (Self.Subpool) Program.Nodes.Null_Literals.Implicit_Null_Literal'
            (Program.Nodes.Null_Literals.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Null_Literals.Null_Literal_Access (Result);
   end Create_Null_Literal;

   function Create_Parenthesized_Expression
    (Self : Element_Factory;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Parenthesized_Expressions
          .Parenthesized_Expression_Access is
      Result : constant Parenthesized_Expression_Access :=

          new (Self.Subpool) Program.Nodes.Parenthesized_Expressions
            .Implicit_Parenthesized_Expression'
            (Program.Nodes.Parenthesized_Expressions.Create
               (Expression => Expression,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Parenthesized_Expressions
        .Parenthesized_Expression_Access
        (Result);
   end Create_Parenthesized_Expression;

   function Create_Raise_Expression
    (Self : Element_Factory;
     Exception_Name       : not null Program.Elements.Expressions
         .Expression_Access;
     Associated_Message   : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Raise_Expressions
          .Raise_Expression_Access is
      Result : constant Raise_Expression_Access :=

          new (Self.Subpool) Program.Nodes.Raise_Expressions
            .Implicit_Raise_Expression'
            (Program.Nodes.Raise_Expressions.Create
               (Exception_Name => Exception_Name,
                Associated_Message => Associated_Message,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Raise_Expressions.Raise_Expression_Access
        (Result);
   end Create_Raise_Expression;

   function Create_Type_Conversion
    (Self : Element_Factory;
     Subtype_Mark         : not null Program.Elements.Expressions
         .Expression_Access;
     Operand              : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Type_Conversions
          .Type_Conversion_Access is
      Result : constant Type_Conversion_Access :=

          new (Self.Subpool) Program.Nodes.Type_Conversions
            .Implicit_Type_Conversion'
            (Program.Nodes.Type_Conversions.Create
               (Subtype_Mark => Subtype_Mark, Operand => Operand,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Type_Conversions.Type_Conversion_Access (Result);
   end Create_Type_Conversion;

   function Create_Qualified_Expression
    (Self : Element_Factory;
     Subtype_Mark         : not null Program.Elements.Expressions
         .Expression_Access;
     Operand              : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Qualified_Expressions
          .Qualified_Expression_Access is
      Result : constant Qualified_Expression_Access :=

          new (Self.Subpool) Program.Nodes.Qualified_Expressions
            .Implicit_Qualified_Expression'
            (Program.Nodes.Qualified_Expressions.Create
               (Subtype_Mark => Subtype_Mark, Operand => Operand,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Qualified_Expressions.Qualified_Expression_Access
        (Result);
   end Create_Qualified_Expression;

   function Create_Allocator
    (Self : Element_Factory;
     Subpool_Name         : Program.Elements.Expressions.Expression_Access;
     Subtype_Indication   : Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Qualified_Expression : Program.Elements.Qualified_Expressions
         .Qualified_Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Allocators.Allocator_Access is
      Result : constant Allocator_Access :=

          new (Self.Subpool) Program.Nodes.Allocators.Implicit_Allocator'
            (Program.Nodes.Allocators.Create
               (Subpool_Name => Subpool_Name,
                Subtype_Indication => Subtype_Indication,
                Qualified_Expression => Qualified_Expression,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Allocators.Allocator_Access (Result);
   end Create_Allocator;

   function Create_Case_Expression
    (Self : Element_Factory;
     Selecting_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Paths                : not null Program.Elements.Case_Expression_Paths
         .Case_Expression_Path_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Case_Expressions
          .Case_Expression_Access is
      Result : constant Case_Expression_Access :=

          new (Self.Subpool) Program.Nodes.Case_Expressions
            .Implicit_Case_Expression'
            (Program.Nodes.Case_Expressions.Create
               (Selecting_Expression => Selecting_Expression, Paths => Paths,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Case_Expressions.Case_Expression_Access (Result);
   end Create_Case_Expression;

   function Create_If_Expression
    (Self : Element_Factory;
     Condition            : not null Program.Elements.Expressions
         .Expression_Access;
     Then_Expression      : not null Program.Elements.Expressions
         .Expression_Access;
     Elsif_Paths          : not null Program.Elements.Elsif_Paths
         .Elsif_Path_Vector_Access;
     Else_Expression      : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.If_Expressions.If_Expression_Access is
      Result : constant If_Expression_Access :=

          new (Self.Subpool) Program.Nodes.If_Expressions
            .Implicit_If_Expression'
            (Program.Nodes.If_Expressions.Create
               (Condition => Condition, Then_Expression => Then_Expression,
                Elsif_Paths => Elsif_Paths, Else_Expression => Else_Expression,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.If_Expressions.If_Expression_Access (Result);
   end Create_If_Expression;

   function Create_Quantified_Expression
    (Self : Element_Factory;
     Parameter            : Program.Elements.Loop_Parameter_Specifications
         .Loop_Parameter_Specification_Access;
     Generalized_Iterator : Program.Elements
         .Generalized_Iterator_Specifications
         .Generalized_Iterator_Specification_Access;
     Element_Iterator     : Program.Elements.Element_Iterator_Specifications
         .Element_Iterator_Specification_Access;
     Predicate            : not null Program.Elements.Expressions
         .Expression_Access;
     Has_All              : Boolean := False;
     Has_Some             : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Quantified_Expressions
          .Quantified_Expression_Access is
      Result : constant Quantified_Expression_Access :=

          new (Self.Subpool) Program.Nodes.Quantified_Expressions
            .Implicit_Quantified_Expression'
            (Program.Nodes.Quantified_Expressions.Create
               (Parameter => Parameter,
                Generalized_Iterator => Generalized_Iterator,
                Element_Iterator => Element_Iterator, Predicate => Predicate,
                Has_All => Has_All, Has_Some => Has_Some,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Quantified_Expressions
        .Quantified_Expression_Access
        (Result);
   end Create_Quantified_Expression;

   function Create_Discriminant_Association
    (Self : Element_Factory;
     Selector_Names       : not null Program.Elements.Identifiers
         .Identifier_Vector_Access;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Discriminant_Associations
          .Discriminant_Association_Access is
      Result : constant Discriminant_Association_Access :=

          new (Self.Subpool) Program.Nodes.Discriminant_Associations
            .Implicit_Discriminant_Association'
            (Program.Nodes.Discriminant_Associations.Create
               (Selector_Names => Selector_Names, Expression => Expression,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Discriminant_Associations
        .Discriminant_Association_Access
        (Result);
   end Create_Discriminant_Association;

   function Create_Record_Component_Association
    (Self : Element_Factory;
     Choices              : not null Program.Element_Vectors
         .Element_Vector_Access;
     Expression           : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Record_Component_Associations
          .Record_Component_Association_Access is
      Result : constant Record_Component_Association_Access :=

          new (Self.Subpool) Program.Nodes.Record_Component_Associations
            .Implicit_Record_Component_Association'
            (Program.Nodes.Record_Component_Associations.Create
               (Choices => Choices, Expression => Expression,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Record_Component_Associations
        .Record_Component_Association_Access
        (Result);
   end Create_Record_Component_Association;

   function Create_Array_Component_Association
    (Self : Element_Factory;
     Choices              : not null Program.Element_Vectors
         .Element_Vector_Access;
     Expression           : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Array_Component_Associations
          .Array_Component_Association_Access is
      Result : constant Array_Component_Association_Access :=

          new (Self.Subpool) Program.Nodes.Array_Component_Associations
            .Implicit_Array_Component_Association'
            (Program.Nodes.Array_Component_Associations.Create
               (Choices => Choices, Expression => Expression,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Array_Component_Associations
        .Array_Component_Association_Access
        (Result);
   end Create_Array_Component_Association;

   function Create_Parameter_Association
    (Self : Element_Factory;
     Formal_Parameter     : Program.Elements.Expressions.Expression_Access;
     Actual_Parameter     : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Parameter_Associations
          .Parameter_Association_Access is
      Result : constant Parameter_Association_Access :=

          new (Self.Subpool) Program.Nodes.Parameter_Associations
            .Implicit_Parameter_Association'
            (Program.Nodes.Parameter_Associations.Create
               (Formal_Parameter => Formal_Parameter,
                Actual_Parameter => Actual_Parameter,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Parameter_Associations
        .Parameter_Association_Access
        (Result);
   end Create_Parameter_Association;

   function Create_Formal_Package_Association
    (Self : Element_Factory;
     Formal_Parameter     : Program.Elements.Expressions.Expression_Access;
     Actual_Parameter     : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Formal_Package_Associations
          .Formal_Package_Association_Access is
      Result : constant Formal_Package_Association_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Package_Associations
            .Implicit_Formal_Package_Association'
            (Program.Nodes.Formal_Package_Associations.Create
               (Formal_Parameter => Formal_Parameter,
                Actual_Parameter => Actual_Parameter,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Formal_Package_Associations
        .Formal_Package_Association_Access
        (Result);
   end Create_Formal_Package_Association;

   function Create_Null_Statement
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Null_Statements.Null_Statement_Access is
      Result : constant Null_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Null_Statements
            .Implicit_Null_Statement'
            (Program.Nodes.Null_Statements.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Null_Statements.Null_Statement_Access (Result);
   end Create_Null_Statement;

   function Create_Assignment_Statement
    (Self : Element_Factory;
     Variable_Name        : not null Program.Elements.Expressions
         .Expression_Access;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Assignment_Statements
          .Assignment_Statement_Access is
      Result : constant Assignment_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Assignment_Statements
            .Implicit_Assignment_Statement'
            (Program.Nodes.Assignment_Statements.Create
               (Variable_Name => Variable_Name, Expression => Expression,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Assignment_Statements.Assignment_Statement_Access
        (Result);
   end Create_Assignment_Statement;

   function Create_If_Statement
    (Self : Element_Factory;
     Condition            : not null Program.Elements.Expressions
         .Expression_Access;
     Then_Statements      : not null Program.Element_Vectors
         .Element_Vector_Access;
     Elsif_Paths          : not null Program.Elements.Elsif_Paths
         .Elsif_Path_Vector_Access;
     Else_Statements      : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.If_Statements.If_Statement_Access is
      Result : constant If_Statement_Access :=

          new (Self.Subpool) Program.Nodes.If_Statements.Implicit_If_Statement'
            (Program.Nodes.If_Statements.Create
               (Condition => Condition, Then_Statements => Then_Statements,
                Elsif_Paths => Elsif_Paths, Else_Statements => Else_Statements,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.If_Statements.If_Statement_Access (Result);
   end Create_If_Statement;

   function Create_Case_Statement
    (Self : Element_Factory;
     Selecting_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Paths                : not null Program.Elements.Case_Paths
         .Case_Path_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Case_Statements.Case_Statement_Access is
      Result : constant Case_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Case_Statements
            .Implicit_Case_Statement'
            (Program.Nodes.Case_Statements.Create
               (Selecting_Expression => Selecting_Expression, Paths => Paths,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Case_Statements.Case_Statement_Access (Result);
   end Create_Case_Statement;

   function Create_Loop_Statement
    (Self : Element_Factory;
     Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit      : Boolean := False;
     Is_Part_Of_Inherited     : Boolean := False;
     Is_Part_Of_Instance      : Boolean := False)
      return not null Program.Elements.Loop_Statements.Loop_Statement_Access is
      Result : constant Loop_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Loop_Statements
            .Implicit_Loop_Statement'
            (Program.Nodes.Loop_Statements.Create
               (Statement_Identifier => Statement_Identifier,
                Statements => Statements,
                End_Statement_Identifier => End_Statement_Identifier,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Loop_Statements.Loop_Statement_Access (Result);
   end Create_Loop_Statement;

   function Create_While_Loop_Statement
    (Self : Element_Factory;
     Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Condition                : not null Program.Elements.Expressions
         .Expression_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit      : Boolean := False;
     Is_Part_Of_Inherited     : Boolean := False;
     Is_Part_Of_Instance      : Boolean := False)
      return not null Program.Elements.While_Loop_Statements
          .While_Loop_Statement_Access is
      Result : constant While_Loop_Statement_Access :=

          new (Self.Subpool) Program.Nodes.While_Loop_Statements
            .Implicit_While_Loop_Statement'
            (Program.Nodes.While_Loop_Statements.Create
               (Statement_Identifier => Statement_Identifier,
                Condition => Condition, Statements => Statements,
                End_Statement_Identifier => End_Statement_Identifier,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.While_Loop_Statements.While_Loop_Statement_Access
        (Result);
   end Create_While_Loop_Statement;

   function Create_For_Loop_Statement
    (Self : Element_Factory;
     Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Loop_Parameter           : Program.Elements.Loop_Parameter_Specifications
         .Loop_Parameter_Specification_Access;
     Generalized_Iterator     : Program.Elements
         .Generalized_Iterator_Specifications
         .Generalized_Iterator_Specification_Access;
     Element_Iterator         : Program.Elements
         .Element_Iterator_Specifications
         .Element_Iterator_Specification_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit      : Boolean := False;
     Is_Part_Of_Inherited     : Boolean := False;
     Is_Part_Of_Instance      : Boolean := False)
      return not null Program.Elements.For_Loop_Statements
          .For_Loop_Statement_Access is
      Result : constant For_Loop_Statement_Access :=

          new (Self.Subpool) Program.Nodes.For_Loop_Statements
            .Implicit_For_Loop_Statement'
            (Program.Nodes.For_Loop_Statements.Create
               (Statement_Identifier => Statement_Identifier,
                Loop_Parameter => Loop_Parameter,
                Generalized_Iterator => Generalized_Iterator,
                Element_Iterator => Element_Iterator, Statements => Statements,
                End_Statement_Identifier => End_Statement_Identifier,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.For_Loop_Statements.For_Loop_Statement_Access
        (Result);
   end Create_For_Loop_Statement;

   function Create_Block_Statement
    (Self : Element_Factory;
     Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Declarations             : not null Program.Element_Vectors
         .Element_Vector_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Handlers       : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit      : Boolean := False;
     Is_Part_Of_Inherited     : Boolean := False;
     Is_Part_Of_Instance      : Boolean := False)
      return not null Program.Elements.Block_Statements
          .Block_Statement_Access is
      Result : constant Block_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Block_Statements
            .Implicit_Block_Statement'
            (Program.Nodes.Block_Statements.Create
               (Statement_Identifier => Statement_Identifier,
                Declarations => Declarations, Statements => Statements,
                Exception_Handlers => Exception_Handlers,
                End_Statement_Identifier => End_Statement_Identifier,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Block_Statements.Block_Statement_Access (Result);
   end Create_Block_Statement;

   function Create_Exit_Statement
    (Self : Element_Factory;
     Exit_Loop_Name       : Program.Elements.Expressions.Expression_Access;
     Condition            : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Exit_Statements.Exit_Statement_Access is
      Result : constant Exit_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Exit_Statements
            .Implicit_Exit_Statement'
            (Program.Nodes.Exit_Statements.Create
               (Exit_Loop_Name => Exit_Loop_Name, Condition => Condition,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Exit_Statements.Exit_Statement_Access (Result);
   end Create_Exit_Statement;

   function Create_Goto_Statement
    (Self : Element_Factory;
     Goto_Label           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Goto_Statements.Goto_Statement_Access is
      Result : constant Goto_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Goto_Statements
            .Implicit_Goto_Statement'
            (Program.Nodes.Goto_Statements.Create
               (Goto_Label => Goto_Label,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Goto_Statements.Goto_Statement_Access (Result);
   end Create_Goto_Statement;

   function Create_Call_Statement
    (Self : Element_Factory;
     Called_Name          : not null Program.Elements.Expressions
         .Expression_Access;
     Parameters           : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Call_Statements.Call_Statement_Access is
      Result : constant Call_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Call_Statements
            .Implicit_Call_Statement'
            (Program.Nodes.Call_Statements.Create
               (Called_Name => Called_Name, Parameters => Parameters,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Call_Statements.Call_Statement_Access (Result);
   end Create_Call_Statement;

   function Create_Simple_Return_Statement
    (Self : Element_Factory;
     Expression           : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Simple_Return_Statements
          .Simple_Return_Statement_Access is
      Result : constant Simple_Return_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Simple_Return_Statements
            .Implicit_Simple_Return_Statement'
            (Program.Nodes.Simple_Return_Statements.Create
               (Expression => Expression,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Simple_Return_Statements
        .Simple_Return_Statement_Access
        (Result);
   end Create_Simple_Return_Statement;

   function Create_Extended_Return_Statement
    (Self : Element_Factory;
     Return_Object        : not null Program.Elements
         .Return_Object_Specifications.Return_Object_Specification_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Handlers   : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Extended_Return_Statements
          .Extended_Return_Statement_Access is
      Result : constant Extended_Return_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Extended_Return_Statements
            .Implicit_Extended_Return_Statement'
            (Program.Nodes.Extended_Return_Statements.Create
               (Return_Object => Return_Object, Statements => Statements,
                Exception_Handlers => Exception_Handlers,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Extended_Return_Statements
        .Extended_Return_Statement_Access
        (Result);
   end Create_Extended_Return_Statement;

   function Create_Accept_Statement
    (Self : Element_Factory;
     Entry_Name               : not null Program.Elements.Identifiers
         .Identifier_Access;
     Entry_Index              : Program.Elements.Expressions.Expression_Access;
     Parameters               : not null Program.Elements
         .Parameter_Specifications.Parameter_Specification_Vector_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Handlers       : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit      : Boolean := False;
     Is_Part_Of_Inherited     : Boolean := False;
     Is_Part_Of_Instance      : Boolean := False)
      return not null Program.Elements.Accept_Statements
          .Accept_Statement_Access is
      Result : constant Accept_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Accept_Statements
            .Implicit_Accept_Statement'
            (Program.Nodes.Accept_Statements.Create
               (Entry_Name => Entry_Name, Entry_Index => Entry_Index,
                Parameters => Parameters, Statements => Statements,
                Exception_Handlers => Exception_Handlers,
                End_Statement_Identifier => End_Statement_Identifier,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Accept_Statements.Accept_Statement_Access
        (Result);
   end Create_Accept_Statement;

   function Create_Requeue_Statement
    (Self : Element_Factory;
     Entry_Name           : not null Program.Elements.Expressions
         .Expression_Access;
     Has_With_Abort       : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Requeue_Statements
          .Requeue_Statement_Access is
      Result : constant Requeue_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Requeue_Statements
            .Implicit_Requeue_Statement'
            (Program.Nodes.Requeue_Statements.Create
               (Entry_Name => Entry_Name, Has_With_Abort => Has_With_Abort,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Requeue_Statements.Requeue_Statement_Access
        (Result);
   end Create_Requeue_Statement;

   function Create_Delay_Statement
    (Self : Element_Factory;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Delay_Statements
          .Delay_Statement_Access is
      Result : constant Delay_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Delay_Statements
            .Implicit_Delay_Statement'
            (Program.Nodes.Delay_Statements.Create
               (Expression => Expression,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Delay_Statements.Delay_Statement_Access (Result);
   end Create_Delay_Statement;

   function Create_Terminate_Alternative_Statement
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Terminate_Alternative_Statements
          .Terminate_Alternative_Statement_Access is
      Result : constant Terminate_Alternative_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Terminate_Alternative_Statements
            .Implicit_Terminate_Alternative_Statement'
            (Program.Nodes.Terminate_Alternative_Statements.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Terminate_Alternative_Statements
        .Terminate_Alternative_Statement_Access
        (Result);
   end Create_Terminate_Alternative_Statement;

   function Create_Select_Statement
    (Self : Element_Factory;
     Paths                 : not null Program.Elements.Select_Paths
         .Select_Path_Vector_Access;
     Then_Abort_Statements : not null Program.Element_Vectors
         .Element_Vector_Access;
     Else_Statements       : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit   : Boolean := False;
     Is_Part_Of_Inherited  : Boolean := False;
     Is_Part_Of_Instance   : Boolean := False)
      return not null Program.Elements.Select_Statements
          .Select_Statement_Access is
      Result : constant Select_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Select_Statements
            .Implicit_Select_Statement'
            (Program.Nodes.Select_Statements.Create
               (Paths => Paths, Then_Abort_Statements => Then_Abort_Statements,
                Else_Statements => Else_Statements,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Select_Statements.Select_Statement_Access
        (Result);
   end Create_Select_Statement;

   function Create_Abort_Statement
    (Self : Element_Factory;
     Aborted_Tasks        : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Abort_Statements
          .Abort_Statement_Access is
      Result : constant Abort_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Abort_Statements
            .Implicit_Abort_Statement'
            (Program.Nodes.Abort_Statements.Create
               (Aborted_Tasks => Aborted_Tasks,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Abort_Statements.Abort_Statement_Access (Result);
   end Create_Abort_Statement;

   function Create_Raise_Statement
    (Self : Element_Factory;
     Raised_Exception     : Program.Elements.Expressions.Expression_Access;
     Associated_Message   : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Raise_Statements
          .Raise_Statement_Access is
      Result : constant Raise_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Raise_Statements
            .Implicit_Raise_Statement'
            (Program.Nodes.Raise_Statements.Create
               (Raised_Exception => Raised_Exception,
                Associated_Message => Associated_Message,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Raise_Statements.Raise_Statement_Access (Result);
   end Create_Raise_Statement;

   function Create_Code_Statement
    (Self : Element_Factory;
     Expression           : not null Program.Elements.Qualified_Expressions
         .Qualified_Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Code_Statements.Code_Statement_Access is
      Result : constant Code_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Code_Statements
            .Implicit_Code_Statement'
            (Program.Nodes.Code_Statements.Create
               (Expression => Expression,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Code_Statements.Code_Statement_Access (Result);
   end Create_Code_Statement;

   function Create_Elsif_Path
    (Self : Element_Factory;
     Condition            : not null Program.Elements.Expressions
         .Expression_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Elsif_Paths.Elsif_Path_Access is
      Result : constant Elsif_Path_Access :=

          new (Self.Subpool) Program.Nodes.Elsif_Paths.Implicit_Elsif_Path'
            (Program.Nodes.Elsif_Paths.Create
               (Condition => Condition, Statements => Statements,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Elsif_Paths.Elsif_Path_Access (Result);
   end Create_Elsif_Path;

   function Create_Case_Path
    (Self : Element_Factory;
     Choices              : not null Program.Element_Vectors
         .Element_Vector_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Case_Paths.Case_Path_Access is
      Result : constant Case_Path_Access :=

          new (Self.Subpool) Program.Nodes.Case_Paths.Implicit_Case_Path'
            (Program.Nodes.Case_Paths.Create
               (Choices => Choices, Statements => Statements,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Case_Paths.Case_Path_Access (Result);
   end Create_Case_Path;

   function Create_Select_Path
    (Self : Element_Factory;
     Guard                : Program.Elements.Expressions.Expression_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Select_Paths.Select_Path_Access is
      Result : constant Select_Path_Access :=

          new (Self.Subpool) Program.Nodes.Select_Paths.Implicit_Select_Path'
            (Program.Nodes.Select_Paths.Create
               (Guard => Guard, Statements => Statements,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Select_Paths.Select_Path_Access (Result);
   end Create_Select_Path;

   function Create_Case_Expression_Path
    (Self : Element_Factory;
     Choices              : not null Program.Element_Vectors
         .Element_Vector_Access;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Case_Expression_Paths
          .Case_Expression_Path_Access is
      Result : constant Case_Expression_Path_Access :=

          new (Self.Subpool) Program.Nodes.Case_Expression_Paths
            .Implicit_Case_Expression_Path'
            (Program.Nodes.Case_Expression_Paths.Create
               (Choices => Choices, Expression => Expression,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Case_Expression_Paths.Case_Expression_Path_Access
        (Result);
   end Create_Case_Expression_Path;

   function Create_Elsif_Expression_Path
    (Self : Element_Factory;
     Condition            : not null Program.Elements.Expressions
         .Expression_Access;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Elsif_Expression_Paths
          .Elsif_Expression_Path_Access is
      Result : constant Elsif_Expression_Path_Access :=

          new (Self.Subpool) Program.Nodes.Elsif_Expression_Paths
            .Implicit_Elsif_Expression_Path'
            (Program.Nodes.Elsif_Expression_Paths.Create
               (Condition => Condition, Expression => Expression,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Elsif_Expression_Paths
        .Elsif_Expression_Path_Access
        (Result);
   end Create_Elsif_Expression_Path;

   function Create_Use_Clause
    (Self : Element_Factory;
     Clause_Names         : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Has_All              : Boolean := False;
     Has_Type             : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Use_Clauses.Use_Clause_Access is
      Result : constant Use_Clause_Access :=

          new (Self.Subpool) Program.Nodes.Use_Clauses.Implicit_Use_Clause'
            (Program.Nodes.Use_Clauses.Create
               (Clause_Names => Clause_Names, Has_All => Has_All,
                Has_Type => Has_Type,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Use_Clauses.Use_Clause_Access (Result);
   end Create_Use_Clause;

   function Create_With_Clause
    (Self : Element_Factory;
     Clause_Names         : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Has_Limited          : Boolean := False;
     Has_Private          : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.With_Clauses.With_Clause_Access is
      Result : constant With_Clause_Access :=

          new (Self.Subpool) Program.Nodes.With_Clauses.Implicit_With_Clause'
            (Program.Nodes.With_Clauses.Create
               (Clause_Names => Clause_Names, Has_Limited => Has_Limited,
                Has_Private => Has_Private,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.With_Clauses.With_Clause_Access (Result);
   end Create_With_Clause;

   function Create_Component_Clause
    (Self : Element_Factory;
     Clause_Name          : not null Program.Elements.Identifiers
         .Identifier_Access;
     Position             : not null Program.Elements.Expressions
         .Expression_Access;
     Clause_Range         : not null Program.Elements.Simple_Expression_Ranges
         .Simple_Expression_Range_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Component_Clauses
          .Component_Clause_Access is
      Result : constant Component_Clause_Access :=

          new (Self.Subpool) Program.Nodes.Component_Clauses
            .Implicit_Component_Clause'
            (Program.Nodes.Component_Clauses.Create
               (Clause_Name => Clause_Name, Position => Position,
                Clause_Range => Clause_Range,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Component_Clauses.Component_Clause_Access
        (Result);
   end Create_Component_Clause;

   function Create_Derived_Type
    (Self : Element_Factory;
     Parent               : not null Program.Elements.Expressions
         .Expression_Access;
     Has_Abstract         : Boolean := False;
     Has_Limited          : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Derived_Types.Derived_Type_Access is
      Result : constant Derived_Type_Access :=

          new (Self.Subpool) Program.Nodes.Derived_Types.Implicit_Derived_Type'
            (Program.Nodes.Derived_Types.Create
               (Parent => Parent, Has_Abstract => Has_Abstract,
                Has_Limited => Has_Limited,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Derived_Types.Derived_Type_Access (Result);
   end Create_Derived_Type;

   function Create_Derived_Record_Extension
    (Self : Element_Factory;
     Parent               : not null Program.Elements.Expressions
         .Expression_Access;
     Progenitors          : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Record_Definition    : not null Program.Elements.Definitions
         .Definition_Access;
     Has_Abstract         : Boolean := False;
     Has_Limited          : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Derived_Record_Extensions
          .Derived_Record_Extension_Access is
      Result : constant Derived_Record_Extension_Access :=

          new (Self.Subpool) Program.Nodes.Derived_Record_Extensions
            .Implicit_Derived_Record_Extension'
            (Program.Nodes.Derived_Record_Extensions.Create
               (Parent => Parent, Progenitors => Progenitors,
                Record_Definition => Record_Definition,
                Has_Abstract => Has_Abstract, Has_Limited => Has_Limited,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Derived_Record_Extensions
        .Derived_Record_Extension_Access
        (Result);
   end Create_Derived_Record_Extension;

   function Create_Enumeration_Type
    (Self : Element_Factory;
     Literals             : not null Program.Elements
         .Enumeration_Literal_Specifications
         .Enumeration_Literal_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Enumeration_Types
          .Enumeration_Type_Access is
      Result : constant Enumeration_Type_Access :=

          new (Self.Subpool) Program.Nodes.Enumeration_Types
            .Implicit_Enumeration_Type'
            (Program.Nodes.Enumeration_Types.Create
               (Literals => Literals,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Enumeration_Types.Enumeration_Type_Access
        (Result);
   end Create_Enumeration_Type;

   function Create_Signed_Integer_Type
    (Self : Element_Factory;
     Lower_Bound          : not null Program.Elements.Expressions
         .Expression_Access;
     Upper_Bound          : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Signed_Integer_Types
          .Signed_Integer_Type_Access is
      Result : constant Signed_Integer_Type_Access :=

          new (Self.Subpool) Program.Nodes.Signed_Integer_Types
            .Implicit_Signed_Integer_Type'
            (Program.Nodes.Signed_Integer_Types.Create
               (Lower_Bound => Lower_Bound, Upper_Bound => Upper_Bound,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Signed_Integer_Types.Signed_Integer_Type_Access
        (Result);
   end Create_Signed_Integer_Type;

   function Create_Modular_Type
    (Self : Element_Factory;
     Modulus              : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Modular_Types.Modular_Type_Access is
      Result : constant Modular_Type_Access :=

          new (Self.Subpool) Program.Nodes.Modular_Types.Implicit_Modular_Type'
            (Program.Nodes.Modular_Types.Create
               (Modulus => Modulus, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Modular_Types.Modular_Type_Access (Result);
   end Create_Modular_Type;

   function Create_Root_Type
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Root_Types.Root_Type_Access is
      Result : constant Root_Type_Access :=

          new (Self.Subpool) Program.Nodes.Root_Types.Implicit_Root_Type'
            (Program.Nodes.Root_Types.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Root_Types.Root_Type_Access (Result);
   end Create_Root_Type;

   function Create_Floating_Point_Type
    (Self : Element_Factory;
     Digits_Expression    : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range           : Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Floating_Point_Types
          .Floating_Point_Type_Access is
      Result : constant Floating_Point_Type_Access :=

          new (Self.Subpool) Program.Nodes.Floating_Point_Types
            .Implicit_Floating_Point_Type'
            (Program.Nodes.Floating_Point_Types.Create
               (Digits_Expression => Digits_Expression,
                Real_Range => Real_Range,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Floating_Point_Types.Floating_Point_Type_Access
        (Result);
   end Create_Floating_Point_Type;

   function Create_Ordinary_Fixed_Point_Type
    (Self : Element_Factory;
     Delta_Expression     : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range           : not null Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Ordinary_Fixed_Point_Types
          .Ordinary_Fixed_Point_Type_Access is
      Result : constant Ordinary_Fixed_Point_Type_Access :=

          new (Self.Subpool) Program.Nodes.Ordinary_Fixed_Point_Types
            .Implicit_Ordinary_Fixed_Point_Type'
            (Program.Nodes.Ordinary_Fixed_Point_Types.Create
               (Delta_Expression => Delta_Expression, Real_Range => Real_Range,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Ordinary_Fixed_Point_Types
        .Ordinary_Fixed_Point_Type_Access
        (Result);
   end Create_Ordinary_Fixed_Point_Type;

   function Create_Decimal_Fixed_Point_Type
    (Self : Element_Factory;
     Delta_Expression     : not null Program.Elements.Expressions
         .Expression_Access;
     Digits_Expression    : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range           : Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Decimal_Fixed_Point_Types
          .Decimal_Fixed_Point_Type_Access is
      Result : constant Decimal_Fixed_Point_Type_Access :=

          new (Self.Subpool) Program.Nodes.Decimal_Fixed_Point_Types
            .Implicit_Decimal_Fixed_Point_Type'
            (Program.Nodes.Decimal_Fixed_Point_Types.Create
               (Delta_Expression => Delta_Expression,
                Digits_Expression => Digits_Expression,
                Real_Range => Real_Range,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Decimal_Fixed_Point_Types
        .Decimal_Fixed_Point_Type_Access
        (Result);
   end Create_Decimal_Fixed_Point_Type;

   function Create_Unconstrained_Array_Type
    (Self : Element_Factory;
     Index_Subtypes       : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Component_Definition : not null Program.Elements.Component_Definitions
         .Component_Definition_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Unconstrained_Array_Types
          .Unconstrained_Array_Type_Access is
      Result : constant Unconstrained_Array_Type_Access :=

          new (Self.Subpool) Program.Nodes.Unconstrained_Array_Types
            .Implicit_Unconstrained_Array_Type'
            (Program.Nodes.Unconstrained_Array_Types.Create
               (Index_Subtypes => Index_Subtypes,
                Component_Definition => Component_Definition,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Unconstrained_Array_Types
        .Unconstrained_Array_Type_Access
        (Result);
   end Create_Unconstrained_Array_Type;

   function Create_Constrained_Array_Type
    (Self : Element_Factory;
     Index_Subtypes       : not null Program.Elements
         .Discrete_Subtype_Definitions
         .Discrete_Subtype_Definition_Vector_Access;
     Component_Definition : not null Program.Elements.Component_Definitions
         .Component_Definition_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Constrained_Array_Types
          .Constrained_Array_Type_Access is
      Result : constant Constrained_Array_Type_Access :=

          new (Self.Subpool) Program.Nodes.Constrained_Array_Types
            .Implicit_Constrained_Array_Type'
            (Program.Nodes.Constrained_Array_Types.Create
               (Index_Subtypes => Index_Subtypes,
                Component_Definition => Component_Definition,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Constrained_Array_Types
        .Constrained_Array_Type_Access
        (Result);
   end Create_Constrained_Array_Type;

   function Create_Record_Type
    (Self : Element_Factory;
     Record_Definition    : not null Program.Elements.Definitions
         .Definition_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Record_Types.Record_Type_Access is
      Result : constant Record_Type_Access :=

          new (Self.Subpool) Program.Nodes.Record_Types.Implicit_Record_Type'
            (Program.Nodes.Record_Types.Create
               (Record_Definition => Record_Definition,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Record_Types.Record_Type_Access (Result);
   end Create_Record_Type;

   function Create_Interface_Type
    (Self : Element_Factory;
     Progenitors          : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Has_Limited          : Boolean := False;
     Has_Task             : Boolean := False;
     Has_Protected        : Boolean := False;
     Has_Synchronized     : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Interface_Types.Interface_Type_Access is
      Result : constant Interface_Type_Access :=

          new (Self.Subpool) Program.Nodes.Interface_Types
            .Implicit_Interface_Type'
            (Program.Nodes.Interface_Types.Create
               (Progenitors => Progenitors, Has_Limited => Has_Limited,
                Has_Task => Has_Task, Has_Protected => Has_Protected,
                Has_Synchronized => Has_Synchronized,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Interface_Types.Interface_Type_Access (Result);
   end Create_Interface_Type;

   function Create_Object_Access_Type
    (Self : Element_Factory;
     Subtype_Indication   : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Has_Not_Null         : Boolean := False;
     Has_All              : Boolean := False;
     Has_Constant         : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Object_Access_Types
          .Object_Access_Type_Access is
      Result : constant Object_Access_Type_Access :=

          new (Self.Subpool) Program.Nodes.Object_Access_Types
            .Implicit_Object_Access_Type'
            (Program.Nodes.Object_Access_Types.Create
               (Subtype_Indication => Subtype_Indication,
                Has_Not_Null => Has_Not_Null, Has_All => Has_All,
                Has_Constant => Has_Constant,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Object_Access_Types.Object_Access_Type_Access
        (Result);
   end Create_Object_Access_Type;

   function Create_Procedure_Access_Type
    (Self : Element_Factory;
     Parameters           : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Has_Not_Null         : Boolean := False;
     Has_Protected        : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Procedure_Access_Types
          .Procedure_Access_Type_Access is
      Result : constant Procedure_Access_Type_Access :=

          new (Self.Subpool) Program.Nodes.Procedure_Access_Types
            .Implicit_Procedure_Access_Type'
            (Program.Nodes.Procedure_Access_Types.Create
               (Parameters => Parameters, Has_Not_Null => Has_Not_Null,
                Has_Protected => Has_Protected,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Procedure_Access_Types
        .Procedure_Access_Type_Access
        (Result);
   end Create_Procedure_Access_Type;

   function Create_Function_Access_Type
    (Self : Element_Factory;
     Parameters           : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Result_Subtype       : not null Program.Elements.Element_Access;
     Has_Not_Null         : Boolean := False;
     Has_Protected        : Boolean := False;
     Has_Not_Null_2       : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Function_Access_Types
          .Function_Access_Type_Access is
      Result : constant Function_Access_Type_Access :=

          new (Self.Subpool) Program.Nodes.Function_Access_Types
            .Implicit_Function_Access_Type'
            (Program.Nodes.Function_Access_Types.Create
               (Parameters => Parameters, Result_Subtype => Result_Subtype,
                Has_Not_Null => Has_Not_Null, Has_Protected => Has_Protected,
                Has_Not_Null_2 => Has_Not_Null_2,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Function_Access_Types.Function_Access_Type_Access
        (Result);
   end Create_Function_Access_Type;

   function Create_Formal_Private_Type_Definition
    (Self : Element_Factory;
     Has_Abstract         : Boolean := False;
     Has_Tagged           : Boolean := False;
     Has_Limited          : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Formal_Private_Type_Definitions
          .Formal_Private_Type_Definition_Access is
      Result : constant Formal_Private_Type_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Private_Type_Definitions
            .Implicit_Formal_Private_Type_Definition'
            (Program.Nodes.Formal_Private_Type_Definitions.Create
               (Has_Abstract => Has_Abstract, Has_Tagged => Has_Tagged,
                Has_Limited => Has_Limited,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Formal_Private_Type_Definitions
        .Formal_Private_Type_Definition_Access
        (Result);
   end Create_Formal_Private_Type_Definition;

   function Create_Formal_Derived_Type_Definition
    (Self : Element_Factory;
     Subtype_Mark         : not null Program.Elements.Expressions
         .Expression_Access;
     Progenitors          : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Has_Abstract         : Boolean := False;
     Has_Limited          : Boolean := False;
     Has_Synchronized     : Boolean := False;
     Has_With_Private     : Boolean := False;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Formal_Derived_Type_Definitions
          .Formal_Derived_Type_Definition_Access is
      Result : constant Formal_Derived_Type_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Derived_Type_Definitions
            .Implicit_Formal_Derived_Type_Definition'
            (Program.Nodes.Formal_Derived_Type_Definitions.Create
               (Subtype_Mark => Subtype_Mark, Progenitors => Progenitors,
                Has_Abstract => Has_Abstract, Has_Limited => Has_Limited,
                Has_Synchronized => Has_Synchronized,
                Has_With_Private => Has_With_Private,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Formal_Derived_Type_Definitions
        .Formal_Derived_Type_Definition_Access
        (Result);
   end Create_Formal_Derived_Type_Definition;

   function Create_Formal_Discrete_Type_Definition
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Formal_Discrete_Type_Definitions
          .Formal_Discrete_Type_Definition_Access is
      Result : constant Formal_Discrete_Type_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Discrete_Type_Definitions
            .Implicit_Formal_Discrete_Type_Definition'
            (Program.Nodes.Formal_Discrete_Type_Definitions.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Formal_Discrete_Type_Definitions
        .Formal_Discrete_Type_Definition_Access
        (Result);
   end Create_Formal_Discrete_Type_Definition;

   function Create_Formal_Signed_Integer_Type_Definition
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Formal_Signed_Integer_Type_Definitions
          .Formal_Signed_Integer_Type_Definition_Access is
      Result : constant Formal_Signed_Integer_Type_Definition_Access :=

          new (Self.Subpool) Program.Nodes
            .Formal_Signed_Integer_Type_Definitions
            .Implicit_Formal_Signed_Integer_Type_Definition'
            (Program.Nodes.Formal_Signed_Integer_Type_Definitions.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Formal_Signed_Integer_Type_Definitions
        .Formal_Signed_Integer_Type_Definition_Access
        (Result);
   end Create_Formal_Signed_Integer_Type_Definition;

   function Create_Formal_Modular_Type_Definition
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Formal_Modular_Type_Definitions
          .Formal_Modular_Type_Definition_Access is
      Result : constant Formal_Modular_Type_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Modular_Type_Definitions
            .Implicit_Formal_Modular_Type_Definition'
            (Program.Nodes.Formal_Modular_Type_Definitions.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Formal_Modular_Type_Definitions
        .Formal_Modular_Type_Definition_Access
        (Result);
   end Create_Formal_Modular_Type_Definition;

   function Create_Formal_Floating_Point_Definition
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Formal_Floating_Point_Definitions
          .Formal_Floating_Point_Definition_Access is
      Result : constant Formal_Floating_Point_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Floating_Point_Definitions
            .Implicit_Formal_Floating_Point_Definition'
            (Program.Nodes.Formal_Floating_Point_Definitions.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Formal_Floating_Point_Definitions
        .Formal_Floating_Point_Definition_Access
        (Result);
   end Create_Formal_Floating_Point_Definition;

   function Create_Formal_Ordinary_Fixed_Point_Definition
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Formal_Ordinary_Fixed_Point_Definitions
          .Formal_Ordinary_Fixed_Point_Definition_Access is
      Result : constant Formal_Ordinary_Fixed_Point_Definition_Access :=

          new (Self.Subpool) Program.Nodes
            .Formal_Ordinary_Fixed_Point_Definitions
            .Implicit_Formal_Ordinary_Fixed_Point_Definition'
            (Program.Nodes.Formal_Ordinary_Fixed_Point_Definitions.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Formal_Ordinary_Fixed_Point_Definitions
        .Formal_Ordinary_Fixed_Point_Definition_Access
        (Result);
   end Create_Formal_Ordinary_Fixed_Point_Definition;

   function Create_Formal_Decimal_Fixed_Point_Definition
    (Self : Element_Factory;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Formal_Decimal_Fixed_Point_Definitions
          .Formal_Decimal_Fixed_Point_Definition_Access is
      Result : constant Formal_Decimal_Fixed_Point_Definition_Access :=

          new (Self.Subpool) Program.Nodes
            .Formal_Decimal_Fixed_Point_Definitions
            .Implicit_Formal_Decimal_Fixed_Point_Definition'
            (Program.Nodes.Formal_Decimal_Fixed_Point_Definitions.Create
               (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Formal_Decimal_Fixed_Point_Definitions
        .Formal_Decimal_Fixed_Point_Definition_Access
        (Result);
   end Create_Formal_Decimal_Fixed_Point_Definition;

   function Create_Range_Attribute_Reference
    (Self : Element_Factory;
     Range_Attribute      : not null Program.Elements.Attribute_References
         .Attribute_Reference_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Range_Attribute_References
          .Range_Attribute_Reference_Access is
      Result : constant Range_Attribute_Reference_Access :=

          new (Self.Subpool) Program.Nodes.Range_Attribute_References
            .Implicit_Range_Attribute_Reference'
            (Program.Nodes.Range_Attribute_References.Create
               (Range_Attribute => Range_Attribute,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Range_Attribute_References
        .Range_Attribute_Reference_Access
        (Result);
   end Create_Range_Attribute_Reference;

   function Create_Simple_Expression_Range
    (Self : Element_Factory;
     Lower_Bound          : not null Program.Elements.Expressions
         .Expression_Access;
     Upper_Bound          : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Simple_Expression_Ranges
          .Simple_Expression_Range_Access is
      Result : constant Simple_Expression_Range_Access :=

          new (Self.Subpool) Program.Nodes.Simple_Expression_Ranges
            .Implicit_Simple_Expression_Range'
            (Program.Nodes.Simple_Expression_Ranges.Create
               (Lower_Bound => Lower_Bound, Upper_Bound => Upper_Bound,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Simple_Expression_Ranges
        .Simple_Expression_Range_Access
        (Result);
   end Create_Simple_Expression_Range;

   function Create_Digits_Constraint
    (Self : Element_Factory;
     Digits_Expression     : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range_Constraint : Program.Elements.Constraints.Constraint_Access;
     Is_Part_Of_Implicit   : Boolean := False;
     Is_Part_Of_Inherited  : Boolean := False;
     Is_Part_Of_Instance   : Boolean := False)
      return not null Program.Elements.Digits_Constraints
          .Digits_Constraint_Access is
      Result : constant Digits_Constraint_Access :=

          new (Self.Subpool) Program.Nodes.Digits_Constraints
            .Implicit_Digits_Constraint'
            (Program.Nodes.Digits_Constraints.Create
               (Digits_Expression => Digits_Expression,
                Real_Range_Constraint => Real_Range_Constraint,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Digits_Constraints.Digits_Constraint_Access
        (Result);
   end Create_Digits_Constraint;

   function Create_Delta_Constraint
    (Self : Element_Factory;
     Delta_Expression      : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range_Constraint : Program.Elements.Constraints.Constraint_Access;
     Is_Part_Of_Implicit   : Boolean := False;
     Is_Part_Of_Inherited  : Boolean := False;
     Is_Part_Of_Instance   : Boolean := False)
      return not null Program.Elements.Delta_Constraints
          .Delta_Constraint_Access is
      Result : constant Delta_Constraint_Access :=

          new (Self.Subpool) Program.Nodes.Delta_Constraints
            .Implicit_Delta_Constraint'
            (Program.Nodes.Delta_Constraints.Create
               (Delta_Expression => Delta_Expression,
                Real_Range_Constraint => Real_Range_Constraint,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Delta_Constraints.Delta_Constraint_Access
        (Result);
   end Create_Delta_Constraint;

   function Create_Index_Constraint
    (Self : Element_Factory;
     Ranges               : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Index_Constraints
          .Index_Constraint_Access is
      Result : constant Index_Constraint_Access :=

          new (Self.Subpool) Program.Nodes.Index_Constraints
            .Implicit_Index_Constraint'
            (Program.Nodes.Index_Constraints.Create
               (Ranges => Ranges, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Index_Constraints.Index_Constraint_Access
        (Result);
   end Create_Index_Constraint;

   function Create_Discriminant_Constraint
    (Self : Element_Factory;
     Discriminants        : not null Program.Elements.Discriminant_Associations
         .Discriminant_Association_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Discriminant_Constraints
          .Discriminant_Constraint_Access is
      Result : constant Discriminant_Constraint_Access :=

          new (Self.Subpool) Program.Nodes.Discriminant_Constraints
            .Implicit_Discriminant_Constraint'
            (Program.Nodes.Discriminant_Constraints.Create
               (Discriminants => Discriminants,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Discriminant_Constraints
        .Discriminant_Constraint_Access
        (Result);
   end Create_Discriminant_Constraint;

   function Create_Attribute_Definition_Clause
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Expressions
         .Expression_Access;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Attribute_Definition_Clauses
          .Attribute_Definition_Clause_Access is
      Result : constant Attribute_Definition_Clause_Access :=

          new (Self.Subpool) Program.Nodes.Attribute_Definition_Clauses
            .Implicit_Attribute_Definition_Clause'
            (Program.Nodes.Attribute_Definition_Clauses.Create
               (Name => Name, Expression => Expression,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Attribute_Definition_Clauses
        .Attribute_Definition_Clause_Access
        (Result);
   end Create_Attribute_Definition_Clause;

   function Create_Enumeration_Representation_Clause
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Expressions
         .Expression_Access;
     Expression           : not null Program.Elements.Array_Aggregates
         .Array_Aggregate_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Enumeration_Representation_Clauses
          .Enumeration_Representation_Clause_Access is
      Result : constant Enumeration_Representation_Clause_Access :=

          new (Self.Subpool) Program.Nodes.Enumeration_Representation_Clauses
            .Implicit_Enumeration_Representation_Clause'
            (Program.Nodes.Enumeration_Representation_Clauses.Create
               (Name => Name, Expression => Expression,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Enumeration_Representation_Clauses
        .Enumeration_Representation_Clause_Access
        (Result);
   end Create_Enumeration_Representation_Clause;

   function Create_Record_Representation_Clause
    (Self : Element_Factory;
     Name                  : not null Program.Elements.Expressions
         .Expression_Access;
     Mod_Clause_Expression : Program.Elements.Expressions.Expression_Access;
     Component_Clauses     : not null Program.Elements.Component_Clauses
         .Component_Clause_Vector_Access;
     Is_Part_Of_Implicit   : Boolean := False;
     Is_Part_Of_Inherited  : Boolean := False;
     Is_Part_Of_Instance   : Boolean := False)
      return not null Program.Elements.Record_Representation_Clauses
          .Record_Representation_Clause_Access is
      Result : constant Record_Representation_Clause_Access :=

          new (Self.Subpool) Program.Nodes.Record_Representation_Clauses
            .Implicit_Record_Representation_Clause'
            (Program.Nodes.Record_Representation_Clauses.Create
               (Name => Name, Mod_Clause_Expression => Mod_Clause_Expression,
                Component_Clauses => Component_Clauses,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Record_Representation_Clauses
        .Record_Representation_Clause_Access
        (Result);
   end Create_Record_Representation_Clause;

   function Create_At_Clause
    (Self : Element_Factory;
     Name                 : not null Program.Elements.Identifiers
         .Identifier_Access;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.At_Clauses.At_Clause_Access is
      Result : constant At_Clause_Access :=

          new (Self.Subpool) Program.Nodes.At_Clauses.Implicit_At_Clause'
            (Program.Nodes.At_Clauses.Create
               (Name => Name, Expression => Expression,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.At_Clauses.At_Clause_Access (Result);
   end Create_At_Clause;

   function Create_Exception_Handler
    (Self : Element_Factory;
     Choice_Parameter     : Program.Elements.Choice_Parameter_Specifications
         .Choice_Parameter_Specification_Access;
     Choices              : not null Program.Element_Vectors
         .Element_Vector_Access;
     Statements           : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return not null Program.Elements.Exception_Handlers
          .Exception_Handler_Access is
      Result : constant Exception_Handler_Access :=

          new (Self.Subpool) Program.Nodes.Exception_Handlers
            .Implicit_Exception_Handler'
            (Program.Nodes.Exception_Handlers.Create
               (Choice_Parameter => Choice_Parameter, Choices => Choices,
                Statements => Statements,
                Is_Part_Of_Implicit => Is_Part_Of_Implicit,
                Is_Part_Of_Inherited => Is_Part_Of_Inherited,
                Is_Part_Of_Instance => Is_Part_Of_Instance));
   begin
      return Program.Elements.Exception_Handlers.Exception_Handler_Access
        (Result);
   end Create_Exception_Handler;

end Program.Implicit_Element_Factories;
