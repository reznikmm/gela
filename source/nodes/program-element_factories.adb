--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Storage_Pools;
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

package body Program.Element_Factories is

   type Pragma_Access is not null access Program.Nodes.Pragmas.Pragma_Element
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Defining_Identifier_Access is
     not null access Program.Nodes.Defining_Identifiers.Defining_Identifier
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Defining_Character_Literal_Access is
     not null access Program.Nodes.Defining_Character_Literals
       .Defining_Character_Literal
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Defining_Operator_Symbol_Access is
     not null access Program.Nodes.Defining_Operator_Symbols
       .Defining_Operator_Symbol
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Defining_Expanded_Name_Access is
     not null access Program.Nodes.Defining_Expanded_Names
       .Defining_Expanded_Name with Storage_Pool => Program.Storage_Pools.Pool;

   type Type_Declaration_Access is
     not null access Program.Nodes.Type_Declarations.Type_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Task_Type_Declaration_Access is
     not null access Program.Nodes.Task_Type_Declarations.Task_Type_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Protected_Type_Declaration_Access is
     not null access Program.Nodes.Protected_Type_Declarations
       .Protected_Type_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Subtype_Declaration_Access is
     not null access Program.Nodes.Subtype_Declarations.Subtype_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Object_Declaration_Access is
     not null access Program.Nodes.Object_Declarations.Object_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Single_Task_Declaration_Access is
     not null access Program.Nodes.Single_Task_Declarations
       .Single_Task_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Single_Protected_Declaration_Access is
     not null access Program.Nodes.Single_Protected_Declarations
       .Single_Protected_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Number_Declaration_Access is
     not null access Program.Nodes.Number_Declarations.Number_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Enumeration_Literal_Specification_Access is
     not null access Program.Nodes.Enumeration_Literal_Specifications
       .Enumeration_Literal_Specification
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Discriminant_Specification_Access is
     not null access Program.Nodes.Discriminant_Specifications
       .Discriminant_Specification
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Component_Declaration_Access is
     not null access Program.Nodes.Component_Declarations.Component_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Loop_Parameter_Specification_Access is
     not null access Program.Nodes.Loop_Parameter_Specifications
       .Loop_Parameter_Specification
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Generalized_Iterator_Specification_Access is
     not null access Program.Nodes.Generalized_Iterator_Specifications
       .Generalized_Iterator_Specification
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Element_Iterator_Specification_Access is
     not null access Program.Nodes.Element_Iterator_Specifications
       .Element_Iterator_Specification
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Procedure_Declaration_Access is
     not null access Program.Nodes.Procedure_Declarations.Procedure_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Function_Declaration_Access is
     not null access Program.Nodes.Function_Declarations.Function_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Parameter_Specification_Access is
     not null access Program.Nodes.Parameter_Specifications
       .Parameter_Specification
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Procedure_Body_Declaration_Access is
     not null access Program.Nodes.Procedure_Body_Declarations
       .Procedure_Body_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Function_Body_Declaration_Access is
     not null access Program.Nodes.Function_Body_Declarations
       .Function_Body_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Return_Object_Specification_Access is
     not null access Program.Nodes.Return_Object_Specifications
       .Return_Object_Specification
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Package_Declaration_Access is
     not null access Program.Nodes.Package_Declarations.Package_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Package_Body_Declaration_Access is
     not null access Program.Nodes.Package_Body_Declarations
       .Package_Body_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Object_Renaming_Declaration_Access is
     not null access Program.Nodes.Object_Renaming_Declarations
       .Object_Renaming_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Exception_Renaming_Declaration_Access is
     not null access Program.Nodes.Exception_Renaming_Declarations
       .Exception_Renaming_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Procedure_Renaming_Declaration_Access is
     not null access Program.Nodes.Procedure_Renaming_Declarations
       .Procedure_Renaming_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Function_Renaming_Declaration_Access is
     not null access Program.Nodes.Function_Renaming_Declarations
       .Function_Renaming_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Package_Renaming_Declaration_Access is
     not null access Program.Nodes.Package_Renaming_Declarations
       .Package_Renaming_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Generic_Package_Renaming_Declaration_Access is
     not null access Program.Nodes.Generic_Package_Renaming_Declarations
       .Generic_Package_Renaming_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Generic_Procedure_Renaming_Declaration_Access is
     not null access Program.Nodes.Generic_Procedure_Renaming_Declarations
       .Generic_Procedure_Renaming_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Generic_Function_Renaming_Declaration_Access is
     not null access Program.Nodes.Generic_Function_Renaming_Declarations
       .Generic_Function_Renaming_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Task_Body_Declaration_Access is
     not null access Program.Nodes.Task_Body_Declarations.Task_Body_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Protected_Body_Declaration_Access is
     not null access Program.Nodes.Protected_Body_Declarations
       .Protected_Body_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Entry_Declaration_Access is
     not null access Program.Nodes.Entry_Declarations.Entry_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Entry_Body_Declaration_Access is
     not null access Program.Nodes.Entry_Body_Declarations
       .Entry_Body_Declaration with Storage_Pool => Program.Storage_Pools.Pool;

   type Entry_Index_Specification_Access is
     not null access Program.Nodes.Entry_Index_Specifications
       .Entry_Index_Specification
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Procedure_Body_Stub_Access is
     not null access Program.Nodes.Procedure_Body_Stubs.Procedure_Body_Stub
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Function_Body_Stub_Access is
     not null access Program.Nodes.Function_Body_Stubs.Function_Body_Stub
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Package_Body_Stub_Access is
     not null access Program.Nodes.Package_Body_Stubs.Package_Body_Stub
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Task_Body_Stub_Access is
     not null access Program.Nodes.Task_Body_Stubs.Task_Body_Stub
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Protected_Body_Stub_Access is
     not null access Program.Nodes.Protected_Body_Stubs.Protected_Body_Stub
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Exception_Declaration_Access is
     not null access Program.Nodes.Exception_Declarations.Exception_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Choice_Parameter_Specification_Access is
     not null access Program.Nodes.Choice_Parameter_Specifications
       .Choice_Parameter_Specification
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Generic_Package_Declaration_Access is
     not null access Program.Nodes.Generic_Package_Declarations
       .Generic_Package_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Generic_Procedure_Declaration_Access is
     not null access Program.Nodes.Generic_Procedure_Declarations
       .Generic_Procedure_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Generic_Function_Declaration_Access is
     not null access Program.Nodes.Generic_Function_Declarations
       .Generic_Function_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Package_Instantiation_Access is
     not null access Program.Nodes.Package_Instantiations.Package_Instantiation
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Procedure_Instantiation_Access is
     not null access Program.Nodes.Procedure_Instantiations
       .Procedure_Instantiation
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Function_Instantiation_Access is
     not null access Program.Nodes.Function_Instantiations
       .Function_Instantiation with Storage_Pool => Program.Storage_Pools.Pool;

   type Formal_Object_Declaration_Access is
     not null access Program.Nodes.Formal_Object_Declarations
       .Formal_Object_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Formal_Type_Declaration_Access is
     not null access Program.Nodes.Formal_Type_Declarations
       .Formal_Type_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Formal_Procedure_Declaration_Access is
     not null access Program.Nodes.Formal_Procedure_Declarations
       .Formal_Procedure_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Formal_Function_Declaration_Access is
     not null access Program.Nodes.Formal_Function_Declarations
       .Formal_Function_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Formal_Package_Declaration_Access is
     not null access Program.Nodes.Formal_Package_Declarations
       .Formal_Package_Declaration
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Subtype_Indication_Access is
     not null access Program.Nodes.Subtype_Indications.Subtype_Indication
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Component_Definition_Access is
     not null access Program.Nodes.Component_Definitions.Component_Definition
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Unknown_Discriminant_Part_Access is
     not null access Program.Nodes.Unknown_Discriminant_Parts
       .Unknown_Discriminant_Part
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Known_Discriminant_Part_Access is
     not null access Program.Nodes.Known_Discriminant_Parts
       .Known_Discriminant_Part
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Record_Definition_Access is
     not null access Program.Nodes.Record_Definitions.Record_Definition
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Null_Component_Access is
     not null access Program.Nodes.Null_Components.Null_Component
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Variant_Part_Access is
     not null access Program.Nodes.Variant_Parts.Variant_Part
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Variant_Access is not null access Program.Nodes.Variants.Variant
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Others_Choice_Access is
     not null access Program.Nodes.Others_Choices.Others_Choice
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Private_Type_Definition_Access is
     not null access Program.Nodes.Private_Type_Definitions
       .Private_Type_Definition
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Private_Extension_Definition_Access is
     not null access Program.Nodes.Private_Extension_Definitions
       .Private_Extension_Definition
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Incomplete_Type_Definition_Access is
     not null access Program.Nodes.Incomplete_Type_Definitions
       .Incomplete_Type_Definition
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Task_Definition_Access is
     not null access Program.Nodes.Task_Definitions.Task_Definition
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Protected_Definition_Access is
     not null access Program.Nodes.Protected_Definitions.Protected_Definition
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Aspect_Specification_Access is
     not null access Program.Nodes.Aspect_Specifications.Aspect_Specification
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Real_Range_Specification_Access is
     not null access Program.Nodes.Real_Range_Specifications
       .Real_Range_Specification
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Numeric_Literal_Access is
     not null access Program.Nodes.Numeric_Literals.Numeric_Literal
     with Storage_Pool => Program.Storage_Pools.Pool;

   type String_Literal_Access is
     not null access Program.Nodes.String_Literals.String_Literal
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Identifier_Access is
     not null access Program.Nodes.Identifiers.Identifier
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Operator_Symbol_Access is
     not null access Program.Nodes.Operator_Symbols.Operator_Symbol
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Character_Literal_Access is
     not null access Program.Nodes.Character_Literals.Character_Literal
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Explicit_Dereference_Access is
     not null access Program.Nodes.Explicit_Dereferences.Explicit_Dereference
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Function_Call_Access is
     not null access Program.Nodes.Function_Calls.Function_Call
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Indexed_Component_Access is
     not null access Program.Nodes.Indexed_Components.Indexed_Component
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Slice_Access is not null access Program.Nodes.Slices.Slice
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Selected_Component_Access is
     not null access Program.Nodes.Selected_Components.Selected_Component
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Attribute_Reference_Access is
     not null access Program.Nodes.Attribute_References.Attribute_Reference
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Record_Aggregate_Access is
     not null access Program.Nodes.Record_Aggregates.Record_Aggregate
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Extension_Aggregate_Access is
     not null access Program.Nodes.Extension_Aggregates.Extension_Aggregate
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Array_Aggregate_Access is
     not null access Program.Nodes.Array_Aggregates.Array_Aggregate
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Short_Circuit_Operation_Access is
     not null access Program.Nodes.Short_Circuit_Operations
       .Short_Circuit_Operation
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Membership_Test_Access is
     not null access Program.Nodes.Membership_Tests.Membership_Test
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Null_Literal_Access is
     not null access Program.Nodes.Null_Literals.Null_Literal
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Parenthesized_Expression_Access is
     not null access Program.Nodes.Parenthesized_Expressions
       .Parenthesized_Expression
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Raise_Expression_Access is
     not null access Program.Nodes.Raise_Expressions.Raise_Expression
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Type_Conversion_Access is
     not null access Program.Nodes.Type_Conversions.Type_Conversion
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Qualified_Expression_Access is
     not null access Program.Nodes.Qualified_Expressions.Qualified_Expression
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Allocator_Access is not null access Program.Nodes.Allocators.Allocator
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Case_Expression_Access is
     not null access Program.Nodes.Case_Expressions.Case_Expression
     with Storage_Pool => Program.Storage_Pools.Pool;

   type If_Expression_Access is
     not null access Program.Nodes.If_Expressions.If_Expression
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Quantified_Expression_Access is
     not null access Program.Nodes.Quantified_Expressions.Quantified_Expression
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Discriminant_Association_Access is
     not null access Program.Nodes.Discriminant_Associations
       .Discriminant_Association
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Record_Component_Association_Access is
     not null access Program.Nodes.Record_Component_Associations
       .Record_Component_Association
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Array_Component_Association_Access is
     not null access Program.Nodes.Array_Component_Associations
       .Array_Component_Association
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Parameter_Association_Access is
     not null access Program.Nodes.Parameter_Associations.Parameter_Association
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Formal_Package_Association_Access is
     not null access Program.Nodes.Formal_Package_Associations
       .Formal_Package_Association
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Null_Statement_Access is
     not null access Program.Nodes.Null_Statements.Null_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Assignment_Statement_Access is
     not null access Program.Nodes.Assignment_Statements.Assignment_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type If_Statement_Access is
     not null access Program.Nodes.If_Statements.If_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Case_Statement_Access is
     not null access Program.Nodes.Case_Statements.Case_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Loop_Statement_Access is
     not null access Program.Nodes.Loop_Statements.Loop_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type While_Loop_Statement_Access is
     not null access Program.Nodes.While_Loop_Statements.While_Loop_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type For_Loop_Statement_Access is
     not null access Program.Nodes.For_Loop_Statements.For_Loop_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Block_Statement_Access is
     not null access Program.Nodes.Block_Statements.Block_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Exit_Statement_Access is
     not null access Program.Nodes.Exit_Statements.Exit_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Goto_Statement_Access is
     not null access Program.Nodes.Goto_Statements.Goto_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Call_Statement_Access is
     not null access Program.Nodes.Call_Statements.Call_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Simple_Return_Statement_Access is
     not null access Program.Nodes.Simple_Return_Statements
       .Simple_Return_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Extended_Return_Statement_Access is
     not null access Program.Nodes.Extended_Return_Statements
       .Extended_Return_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Accept_Statement_Access is
     not null access Program.Nodes.Accept_Statements.Accept_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Requeue_Statement_Access is
     not null access Program.Nodes.Requeue_Statements.Requeue_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Delay_Statement_Access is
     not null access Program.Nodes.Delay_Statements.Delay_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Terminate_Alternative_Statement_Access is
     not null access Program.Nodes.Terminate_Alternative_Statements
       .Terminate_Alternative_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Select_Statement_Access is
     not null access Program.Nodes.Select_Statements.Select_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Abort_Statement_Access is
     not null access Program.Nodes.Abort_Statements.Abort_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Raise_Statement_Access is
     not null access Program.Nodes.Raise_Statements.Raise_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Code_Statement_Access is
     not null access Program.Nodes.Code_Statements.Code_Statement
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Elsif_Path_Access is
     not null access Program.Nodes.Elsif_Paths.Elsif_Path
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Case_Path_Access is not null access Program.Nodes.Case_Paths.Case_Path
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Select_Path_Access is
     not null access Program.Nodes.Select_Paths.Select_Path
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Case_Expression_Path_Access is
     not null access Program.Nodes.Case_Expression_Paths.Case_Expression_Path
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Elsif_Expression_Path_Access is
     not null access Program.Nodes.Elsif_Expression_Paths.Elsif_Expression_Path
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Use_Clause_Access is
     not null access Program.Nodes.Use_Clauses.Use_Clause
     with Storage_Pool => Program.Storage_Pools.Pool;

   type With_Clause_Access is
     not null access Program.Nodes.With_Clauses.With_Clause
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Component_Clause_Access is
     not null access Program.Nodes.Component_Clauses.Component_Clause
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Derived_Type_Access is
     not null access Program.Nodes.Derived_Types.Derived_Type
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Derived_Record_Extension_Access is
     not null access Program.Nodes.Derived_Record_Extensions
       .Derived_Record_Extension
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Enumeration_Type_Access is
     not null access Program.Nodes.Enumeration_Types.Enumeration_Type
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Signed_Integer_Type_Access is
     not null access Program.Nodes.Signed_Integer_Types.Signed_Integer_Type
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Modular_Type_Access is
     not null access Program.Nodes.Modular_Types.Modular_Type
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Root_Type_Access is not null access Program.Nodes.Root_Types.Root_Type
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Floating_Point_Type_Access is
     not null access Program.Nodes.Floating_Point_Types.Floating_Point_Type
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Ordinary_Fixed_Point_Type_Access is
     not null access Program.Nodes.Ordinary_Fixed_Point_Types
       .Ordinary_Fixed_Point_Type
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Decimal_Fixed_Point_Type_Access is
     not null access Program.Nodes.Decimal_Fixed_Point_Types
       .Decimal_Fixed_Point_Type
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Unconstrained_Array_Type_Access is
     not null access Program.Nodes.Unconstrained_Array_Types
       .Unconstrained_Array_Type
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Constrained_Array_Type_Access is
     not null access Program.Nodes.Constrained_Array_Types
       .Constrained_Array_Type with Storage_Pool => Program.Storage_Pools.Pool;

   type Record_Type_Access is
     not null access Program.Nodes.Record_Types.Record_Type
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Interface_Type_Access is
     not null access Program.Nodes.Interface_Types.Interface_Type
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Object_Access_Type_Access is
     not null access Program.Nodes.Object_Access_Types.Object_Access_Type
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Procedure_Access_Type_Access is
     not null access Program.Nodes.Procedure_Access_Types.Procedure_Access_Type
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Function_Access_Type_Access is
     not null access Program.Nodes.Function_Access_Types.Function_Access_Type
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Formal_Private_Type_Definition_Access is
     not null access Program.Nodes.Formal_Private_Type_Definitions
       .Formal_Private_Type_Definition
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Formal_Derived_Type_Definition_Access is
     not null access Program.Nodes.Formal_Derived_Type_Definitions
       .Formal_Derived_Type_Definition
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Formal_Discrete_Type_Definition_Access is
     not null access Program.Nodes.Formal_Discrete_Type_Definitions
       .Formal_Discrete_Type_Definition
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Formal_Signed_Integer_Type_Definition_Access is
     not null access Program.Nodes.Formal_Signed_Integer_Type_Definitions
       .Formal_Signed_Integer_Type_Definition
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Formal_Modular_Type_Definition_Access is
     not null access Program.Nodes.Formal_Modular_Type_Definitions
       .Formal_Modular_Type_Definition
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Formal_Floating_Point_Definition_Access is
     not null access Program.Nodes.Formal_Floating_Point_Definitions
       .Formal_Floating_Point_Definition
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Formal_Ordinary_Fixed_Point_Definition_Access is
     not null access Program.Nodes.Formal_Ordinary_Fixed_Point_Definitions
       .Formal_Ordinary_Fixed_Point_Definition
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Formal_Decimal_Fixed_Point_Definition_Access is
     not null access Program.Nodes.Formal_Decimal_Fixed_Point_Definitions
       .Formal_Decimal_Fixed_Point_Definition
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Range_Attribute_Reference_Access is
     not null access Program.Nodes.Range_Attribute_References
       .Range_Attribute_Reference
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Simple_Expression_Range_Access is
     not null access Program.Nodes.Simple_Expression_Ranges
       .Simple_Expression_Range
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Digits_Constraint_Access is
     not null access Program.Nodes.Digits_Constraints.Digits_Constraint
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Delta_Constraint_Access is
     not null access Program.Nodes.Delta_Constraints.Delta_Constraint
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Index_Constraint_Access is
     not null access Program.Nodes.Index_Constraints.Index_Constraint
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Discriminant_Constraint_Access is
     not null access Program.Nodes.Discriminant_Constraints
       .Discriminant_Constraint
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Attribute_Definition_Clause_Access is
     not null access Program.Nodes.Attribute_Definition_Clauses
       .Attribute_Definition_Clause
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Enumeration_Representation_Clause_Access is
     not null access Program.Nodes.Enumeration_Representation_Clauses
       .Enumeration_Representation_Clause
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Record_Representation_Clause_Access is
     not null access Program.Nodes.Record_Representation_Clauses
       .Record_Representation_Clause
     with Storage_Pool => Program.Storage_Pools.Pool;

   type At_Clause_Access is not null access Program.Nodes.At_Clauses.At_Clause
     with Storage_Pool => Program.Storage_Pools.Pool;

   type Exception_Handler_Access is
     not null access Program.Nodes.Exception_Handlers.Exception_Handler
     with Storage_Pool => Program.Storage_Pools.Pool;

   not overriding function Create_Pragma
    (Self : Element_Factory;
     Pragma_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Identifiers
         .Identifier_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Arguments           : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Pragmas.Pragma_Access is
      Result : constant Pragma_Access :=

          new (Self.Subpool) Program.Nodes.Pragmas.Pragma_Element'
            (Program.Nodes.Pragmas.Create
               (Pragma_Token => Pragma_Token, Name => Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Arguments => Arguments,
                Right_Bracket_Token => Right_Bracket_Token,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Pragmas.Pragma_Access (Result);
   end Create_Pragma;

   not overriding function Create_Defining_Identifier
    (Self             : Element_Factory;
     Identifier_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
      Result : constant Defining_Identifier_Access :=

          new (Self.Subpool) Program.Nodes.Defining_Identifiers
            .Defining_Identifier'
            (Program.Nodes.Defining_Identifiers.Create
               (Identifier_Token => Identifier_Token));
   begin
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
        (Result);
   end Create_Defining_Identifier;

   not overriding function Create_Defining_Character_Literal
    (Self                    : Element_Factory;
     Character_Literal_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Defining_Character_Literals
          .Defining_Character_Literal_Access is
      Result : constant Defining_Character_Literal_Access :=

          new (Self.Subpool) Program.Nodes.Defining_Character_Literals
            .Defining_Character_Literal'
            (Program.Nodes.Defining_Character_Literals.Create
               (Character_Literal_Token => Character_Literal_Token));
   begin
      return Program.Elements.Defining_Character_Literals
        .Defining_Character_Literal_Access
        (Result);
   end Create_Defining_Character_Literal;

   not overriding function Create_Defining_Operator_Symbol
    (Self                  : Element_Factory;
     Operator_Symbol_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Defining_Operator_Symbols
          .Defining_Operator_Symbol_Access is
      Result : constant Defining_Operator_Symbol_Access :=

          new (Self.Subpool) Program.Nodes.Defining_Operator_Symbols
            .Defining_Operator_Symbol'
            (Program.Nodes.Defining_Operator_Symbols.Create
               (Operator_Symbol_Token => Operator_Symbol_Token));
   begin
      return Program.Elements.Defining_Operator_Symbols
        .Defining_Operator_Symbol_Access
        (Result);
   end Create_Defining_Operator_Symbol;

   not overriding function Create_Defining_Expanded_Name
    (Self : Element_Factory;
     Prefix    : not null Program.Elements.Expressions.Expression_Access;
     Dot_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Selector  : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access)
      return not null Program.Elements.Defining_Expanded_Names
          .Defining_Expanded_Name_Access is
      Result : constant Defining_Expanded_Name_Access :=

          new (Self.Subpool) Program.Nodes.Defining_Expanded_Names
            .Defining_Expanded_Name'
            (Program.Nodes.Defining_Expanded_Names.Create
               (Prefix => Prefix, Dot_Token => Dot_Token,
                Selector => Selector));
   begin
      return Program.Elements.Defining_Expanded_Names
        .Defining_Expanded_Name_Access
        (Result);
   end Create_Defining_Expanded_Name;

   not overriding function Create_Type_Declaration
    (Self : Element_Factory;
     Type_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name              : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Discriminant_Part : Program.Elements.Definitions.Definition_Access;
     Is_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Definition        : not null Program.Elements.Definitions
         .Definition_Access;
     With_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects           : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Type_Declarations
          .Type_Declaration_Access is
      Result : constant Type_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Type_Declarations.Type_Declaration'
            (Program.Nodes.Type_Declarations.Create
               (Type_Token => Type_Token, Name => Name,
                Discriminant_Part => Discriminant_Part, Is_Token => Is_Token,
                Definition => Definition, With_Token => With_Token,
                Aspects => Aspects, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Type_Declarations.Type_Declaration_Access
        (Result);
   end Create_Type_Declaration;

   not overriding function Create_Task_Type_Declaration
    (Self : Element_Factory;
     Task_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Type_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name              : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Discriminant_Part : Program.Elements.Known_Discriminant_Parts
         .Known_Discriminant_Part_Access;
     With_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects           : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors       : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     With_Token_2      : Program.Lexical_Elements.Lexical_Element_Access;
     Definition        : not null Program.Elements.Task_Definitions
         .Task_Definition_Access;
     Semicolon_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Task_Type_Declarations
          .Task_Type_Declaration_Access is
      Result : constant Task_Type_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Task_Type_Declarations
            .Task_Type_Declaration'
            (Program.Nodes.Task_Type_Declarations.Create
               (Task_Token => Task_Token, Type_Token => Type_Token,
                Name => Name, Discriminant_Part => Discriminant_Part,
                With_Token => With_Token, Aspects => Aspects,
                Is_Token => Is_Token, New_Token => New_Token,
                Progenitors => Progenitors, With_Token_2 => With_Token_2,
                Definition => Definition, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Task_Type_Declarations
        .Task_Type_Declaration_Access
        (Result);
   end Create_Task_Type_Declaration;

   not overriding function Create_Protected_Type_Declaration
    (Self : Element_Factory;
     Protected_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Type_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name              : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Discriminant_Part : Program.Elements.Known_Discriminant_Parts
         .Known_Discriminant_Part_Access;
     With_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects           : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors       : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     With_Token_2      : Program.Lexical_Elements.Lexical_Element_Access;
     Definition        : not null Program.Elements.Protected_Definitions
         .Protected_Definition_Access;
     Semicolon_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Protected_Type_Declarations
          .Protected_Type_Declaration_Access is
      Result : constant Protected_Type_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Protected_Type_Declarations
            .Protected_Type_Declaration'
            (Program.Nodes.Protected_Type_Declarations.Create
               (Protected_Token => Protected_Token, Type_Token => Type_Token,
                Name => Name, Discriminant_Part => Discriminant_Part,
                With_Token => With_Token, Aspects => Aspects,
                Is_Token => Is_Token, New_Token => New_Token,
                Progenitors => Progenitors, With_Token_2 => With_Token_2,
                Definition => Definition, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Protected_Type_Declarations
        .Protected_Type_Declaration_Access
        (Result);
   end Create_Protected_Type_Declaration;

   not overriding function Create_Subtype_Declaration
    (Self : Element_Factory;
     Subtype_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name               : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Subtype_Indication : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects            : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Subtype_Declarations
          .Subtype_Declaration_Access is
      Result : constant Subtype_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Subtype_Declarations
            .Subtype_Declaration'
            (Program.Nodes.Subtype_Declarations.Create
               (Subtype_Token => Subtype_Token, Name => Name,
                Is_Token => Is_Token, Subtype_Indication => Subtype_Indication,
                With_Token => With_Token, Aspects => Aspects,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Subtype_Declarations.Subtype_Declaration_Access
        (Result);
   end Create_Subtype_Declaration;

   not overriding function Create_Object_Declaration
    (Self : Element_Factory;
     Names                     : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token               : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Aliased_Token             : Program.Lexical_Elements
         .Lexical_Element_Access;
     Constant_Token            : Program.Lexical_Elements
         .Lexical_Element_Access;
     Object_Subtype            : not null Program.Elements.Definitions
         .Definition_Access;
     Assignment_Token          : Program.Lexical_Elements
         .Lexical_Element_Access;
     Initialization_Expression : Program.Elements.Expressions
         .Expression_Access;
     With_Token                : Program.Lexical_Elements
         .Lexical_Element_Access;
     Aspects                   : not null Program.Elements
         .Aspect_Specifications.Aspect_Specification_Vector_Access;
     Semicolon_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Object_Declarations
          .Object_Declaration_Access is
      Result : constant Object_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Object_Declarations
            .Object_Declaration'
            (Program.Nodes.Object_Declarations.Create
               (Names => Names, Colon_Token => Colon_Token,
                Aliased_Token => Aliased_Token,
                Constant_Token => Constant_Token,
                Object_Subtype => Object_Subtype,
                Assignment_Token => Assignment_Token,
                Initialization_Expression => Initialization_Expression,
                With_Token => With_Token, Aspects => Aspects,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Object_Declarations.Object_Declaration_Access
        (Result);
   end Create_Object_Declaration;

   not overriding function Create_Single_Task_Declaration
    (Self : Element_Factory;
     Task_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors     : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     With_Token_2    : Program.Lexical_Elements.Lexical_Element_Access;
     Definition      : not null Program.Elements.Task_Definitions
         .Task_Definition_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Single_Task_Declarations
          .Single_Task_Declaration_Access is
      Result : constant Single_Task_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Single_Task_Declarations
            .Single_Task_Declaration'
            (Program.Nodes.Single_Task_Declarations.Create
               (Task_Token => Task_Token, Name => Name,
                With_Token => With_Token, Aspects => Aspects,
                Is_Token => Is_Token, New_Token => New_Token,
                Progenitors => Progenitors, With_Token_2 => With_Token_2,
                Definition => Definition, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Single_Task_Declarations
        .Single_Task_Declaration_Access
        (Result);
   end Create_Single_Task_Declaration;

   not overriding function Create_Single_Protected_Declaration
    (Self : Element_Factory;
     Protected_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors     : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     With_Token_2    : Program.Lexical_Elements.Lexical_Element_Access;
     Definition      : not null Program.Elements.Protected_Definitions
         .Protected_Definition_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Single_Protected_Declarations
          .Single_Protected_Declaration_Access is
      Result : constant Single_Protected_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Single_Protected_Declarations
            .Single_Protected_Declaration'
            (Program.Nodes.Single_Protected_Declarations.Create
               (Protected_Token => Protected_Token, Name => Name,
                With_Token => With_Token, Aspects => Aspects,
                Is_Token => Is_Token, New_Token => New_Token,
                Progenitors => Progenitors, With_Token_2 => With_Token_2,
                Definition => Definition, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Single_Protected_Declarations
        .Single_Protected_Declaration_Access
        (Result);
   end Create_Single_Protected_Declaration;

   not overriding function Create_Number_Declaration
    (Self : Element_Factory;
     Names            : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Constant_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Assignment_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression       : not null Program.Elements.Expressions
         .Expression_Access;
     Semicolon_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Number_Declarations
          .Number_Declaration_Access is
      Result : constant Number_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Number_Declarations
            .Number_Declaration'
            (Program.Nodes.Number_Declarations.Create
               (Names => Names, Colon_Token => Colon_Token,
                Constant_Token => Constant_Token,
                Assignment_Token => Assignment_Token, Expression => Expression,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Number_Declarations.Number_Declaration_Access
        (Result);
   end Create_Number_Declaration;

   not overriding function Create_Enumeration_Literal_Specification
    (Self : Element_Factory;
     Name : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access)
      return not null Program.Elements.Enumeration_Literal_Specifications
          .Enumeration_Literal_Specification_Access is
      Result : constant Enumeration_Literal_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Enumeration_Literal_Specifications
            .Enumeration_Literal_Specification'
            (Program.Nodes.Enumeration_Literal_Specifications.Create
               (Name => Name));
   begin
      return Program.Elements.Enumeration_Literal_Specifications
        .Enumeration_Literal_Specification_Access
        (Result);
   end Create_Enumeration_Literal_Specification;

   not overriding function Create_Discriminant_Specification
    (Self : Element_Factory;
     Names              : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Object_Subtype     : not null Program.Elements.Element_Access;
     Assignment_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Default_Expression : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Discriminant_Specifications
          .Discriminant_Specification_Access is
      Result : constant Discriminant_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Discriminant_Specifications
            .Discriminant_Specification'
            (Program.Nodes.Discriminant_Specifications.Create
               (Names => Names, Colon_Token => Colon_Token,
                Not_Token => Not_Token, Null_Token => Null_Token,
                Object_Subtype => Object_Subtype,
                Assignment_Token => Assignment_Token,
                Default_Expression => Default_Expression,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Discriminant_Specifications
        .Discriminant_Specification_Access
        (Result);
   end Create_Discriminant_Specification;

   not overriding function Create_Component_Declaration
    (Self : Element_Factory;
     Names              : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Object_Subtype     : not null Program.Elements.Component_Definitions
         .Component_Definition_Access;
     Assignment_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Default_Expression : Program.Elements.Expressions.Expression_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects            : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Component_Declarations
          .Component_Declaration_Access is
      Result : constant Component_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Component_Declarations
            .Component_Declaration'
            (Program.Nodes.Component_Declarations.Create
               (Names => Names, Colon_Token => Colon_Token,
                Object_Subtype => Object_Subtype,
                Assignment_Token => Assignment_Token,
                Default_Expression => Default_Expression,
                With_Token => With_Token, Aspects => Aspects,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Component_Declarations
        .Component_Declaration_Access
        (Result);
   end Create_Component_Declaration;

   not overriding function Create_Loop_Parameter_Specification
    (Self : Element_Factory;
     Name          : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     In_Token      : not null Program.Lexical_Elements.Lexical_Element_Access;
     Reverse_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Definition    : not null Program.Elements.Discrete_Subtype_Definitions
         .Discrete_Subtype_Definition_Access)
      return not null Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification_Access is
      Result : constant Loop_Parameter_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Loop_Parameter_Specifications
            .Loop_Parameter_Specification'
            (Program.Nodes.Loop_Parameter_Specifications.Create
               (Name => Name, In_Token => In_Token,
                Reverse_Token => Reverse_Token, Definition => Definition));
   begin
      return Program.Elements.Loop_Parameter_Specifications
        .Loop_Parameter_Specification_Access
        (Result);
   end Create_Loop_Parameter_Specification;

   not overriding function Create_Generalized_Iterator_Specification
    (Self : Element_Factory;
     Name          : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     In_Token      : not null Program.Lexical_Elements.Lexical_Element_Access;
     Reverse_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Iterator_Name : not null Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification_Access is
      Result : constant Generalized_Iterator_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Generalized_Iterator_Specifications
            .Generalized_Iterator_Specification'
            (Program.Nodes.Generalized_Iterator_Specifications.Create
               (Name => Name, In_Token => In_Token,
                Reverse_Token => Reverse_Token,
                Iterator_Name => Iterator_Name));
   begin
      return Program.Elements.Generalized_Iterator_Specifications
        .Generalized_Iterator_Specification_Access
        (Result);
   end Create_Generalized_Iterator_Specification;

   not overriding function Create_Element_Iterator_Specification
    (Self : Element_Factory;
     Name               : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Subtype_Indication : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Of_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Reverse_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Iterable_Name      : not null Program.Elements.Expressions
         .Expression_Access)
      return not null Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification_Access is
      Result : constant Element_Iterator_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Element_Iterator_Specifications
            .Element_Iterator_Specification'
            (Program.Nodes.Element_Iterator_Specifications.Create
               (Name => Name, Colon_Token => Colon_Token,
                Subtype_Indication => Subtype_Indication, Of_Token => Of_Token,
                Reverse_Token => Reverse_Token,
                Iterable_Name => Iterable_Name));
   begin
      return Program.Elements.Element_Iterator_Specifications
        .Element_Iterator_Specification_Access
        (Result);
   end Create_Element_Iterator_Specification;

   not overriding function Create_Procedure_Declaration
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Is_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Abstract_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Procedure_Declarations
          .Procedure_Declaration_Access is
      Result : constant Procedure_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Procedure_Declarations
            .Procedure_Declaration'
            (Program.Nodes.Procedure_Declarations.Create
               (Not_Token => Not_Token, Overriding_Token => Overriding_Token,
                Procedure_Token => Procedure_Token, Name => Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token,
                Is_Token => Is_Token, Abstract_Token => Abstract_Token,
                With_Token => With_Token, Aspects => Aspects,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Procedure_Declarations
        .Procedure_Declaration_Access
        (Result);
   end Create_Procedure_Declaration;

   not overriding function Create_Function_Declaration
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token_2         : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Subtype      : not null Program.Elements.Element_Access;
     Is_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Expression   : Program.Elements.Parenthesized_Expressions
         .Parenthesized_Expression_Access;
     Abstract_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Function_Declarations
          .Function_Declaration_Access is
      Result : constant Function_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Function_Declarations
            .Function_Declaration'
            (Program.Nodes.Function_Declarations.Create
               (Not_Token => Not_Token, Overriding_Token => Overriding_Token,
                Function_Token => Function_Token, Name => Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token,
                Return_Token => Return_Token, Not_Token_2 => Not_Token_2,
                Null_Token => Null_Token, Result_Subtype => Result_Subtype,
                Is_Token => Is_Token, Result_Expression => Result_Expression,
                Abstract_Token => Abstract_Token, With_Token => With_Token,
                Aspects => Aspects, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Function_Declarations.Function_Declaration_Access
        (Result);
   end Create_Function_Declaration;

   not overriding function Create_Parameter_Specification
    (Self : Element_Factory;
     Names              : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Aliased_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     In_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Out_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Not_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Parameter_Subtype  : not null Program.Elements.Element_Access;
     Assignment_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Default_Expression : Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Parameter_Specifications
          .Parameter_Specification_Access is
      Result : constant Parameter_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Parameter_Specifications
            .Parameter_Specification'
            (Program.Nodes.Parameter_Specifications.Create
               (Names => Names, Colon_Token => Colon_Token,
                Aliased_Token => Aliased_Token, In_Token => In_Token,
                Out_Token => Out_Token, Not_Token => Not_Token,
                Null_Token => Null_Token,
                Parameter_Subtype => Parameter_Subtype,
                Assignment_Token => Assignment_Token,
                Default_Expression => Default_Expression));
   begin
      return Program.Elements.Parameter_Specifications
        .Parameter_Specification_Access
        (Result);
   end Create_Parameter_Specification;

   not overriding function Create_Procedure_Body_Declaration
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Declarations        : not null Program.Element_Vectors
         .Element_Vector_Access;
     Begin_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements          : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Exception_Handlers  : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name            : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Procedure_Body_Declarations
          .Procedure_Body_Declaration_Access is
      Result : constant Procedure_Body_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Procedure_Body_Declarations
            .Procedure_Body_Declaration'
            (Program.Nodes.Procedure_Body_Declarations.Create
               (Not_Token => Not_Token, Overriding_Token => Overriding_Token,
                Procedure_Token => Procedure_Token, Name => Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token,
                With_Token => With_Token, Aspects => Aspects,
                Is_Token => Is_Token, Declarations => Declarations,
                Begin_Token => Begin_Token, Statements => Statements,
                Exception_Token => Exception_Token,
                Exception_Handlers => Exception_Handlers,
                End_Token => End_Token, End_Name => End_Name,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Procedure_Body_Declarations
        .Procedure_Body_Declaration_Access
        (Result);
   end Create_Procedure_Body_Declaration;

   not overriding function Create_Function_Body_Declaration
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token_2         : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Subtype      : not null Program.Elements.Element_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Declarations        : not null Program.Element_Vectors
         .Element_Vector_Access;
     Begin_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements          : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Exception_Handlers  : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name            : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Function_Body_Declarations
          .Function_Body_Declaration_Access is
      Result : constant Function_Body_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Function_Body_Declarations
            .Function_Body_Declaration'
            (Program.Nodes.Function_Body_Declarations.Create
               (Not_Token => Not_Token, Overriding_Token => Overriding_Token,
                Function_Token => Function_Token, Name => Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token,
                Return_Token => Return_Token, Not_Token_2 => Not_Token_2,
                Null_Token => Null_Token, Result_Subtype => Result_Subtype,
                With_Token => With_Token, Aspects => Aspects,
                Is_Token => Is_Token, Declarations => Declarations,
                Begin_Token => Begin_Token, Statements => Statements,
                Exception_Token => Exception_Token,
                Exception_Handlers => Exception_Handlers,
                End_Token => End_Token, End_Name => End_Name,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Function_Body_Declarations
        .Function_Body_Declaration_Access
        (Result);
   end Create_Function_Body_Declaration;

   not overriding function Create_Return_Object_Specification
    (Self : Element_Factory;
     Name             : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Aliased_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Constant_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Object_Subtype   : not null Program.Elements.Element_Access;
     Assignment_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Expression       : Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Return_Object_Specifications
          .Return_Object_Specification_Access is
      Result : constant Return_Object_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Return_Object_Specifications
            .Return_Object_Specification'
            (Program.Nodes.Return_Object_Specifications.Create
               (Name => Name, Colon_Token => Colon_Token,
                Aliased_Token => Aliased_Token,
                Constant_Token => Constant_Token,
                Object_Subtype => Object_Subtype,
                Assignment_Token => Assignment_Token,
                Expression => Expression));
   begin
      return Program.Elements.Return_Object_Specifications
        .Return_Object_Specification_Access
        (Result);
   end Create_Return_Object_Specification;

   not overriding function Create_Package_Declaration
    (Self : Element_Factory;
     Package_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     With_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Visible_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     Private_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name             : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Package_Declarations
          .Package_Declaration_Access is
      Result : constant Package_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Package_Declarations
            .Package_Declaration'
            (Program.Nodes.Package_Declarations.Create
               (Package_Token => Package_Token, Name => Name,
                With_Token => With_Token, Aspects => Aspects,
                Is_Token => Is_Token,
                Visible_Declarations => Visible_Declarations,
                Private_Token => Private_Token,
                Private_Declarations => Private_Declarations,
                End_Token => End_Token, End_Name => End_Name,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Package_Declarations.Package_Declaration_Access
        (Result);
   end Create_Package_Declaration;

   not overriding function Create_Package_Body_Declaration
    (Self : Element_Factory;
     Package_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Body_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name               : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects            : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Declarations       : not null Program.Element_Vectors
         .Element_Vector_Access;
     Begin_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements         : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Exception_Handlers : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name           : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Package_Body_Declarations
          .Package_Body_Declaration_Access is
      Result : constant Package_Body_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Package_Body_Declarations
            .Package_Body_Declaration'
            (Program.Nodes.Package_Body_Declarations.Create
               (Package_Token => Package_Token, Body_Token => Body_Token,
                Name => Name, With_Token => With_Token, Aspects => Aspects,
                Is_Token => Is_Token, Declarations => Declarations,
                Begin_Token => Begin_Token, Statements => Statements,
                Exception_Token => Exception_Token,
                Exception_Handlers => Exception_Handlers,
                End_Token => End_Token, End_Name => End_Name,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Package_Body_Declarations
        .Package_Body_Declaration_Access
        (Result);
   end Create_Package_Body_Declaration;

   not overriding function Create_Object_Renaming_Declaration
    (Self : Element_Factory;
     Names           : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Object_Subtype  : not null Program.Elements.Element_Access;
     Renames_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Renamed_Object  : not null Program.Elements.Expressions.Expression_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Object_Renaming_Declarations
          .Object_Renaming_Declaration_Access is
      Result : constant Object_Renaming_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Object_Renaming_Declarations
            .Object_Renaming_Declaration'
            (Program.Nodes.Object_Renaming_Declarations.Create
               (Names => Names, Colon_Token => Colon_Token,
                Not_Token => Not_Token, Null_Token => Null_Token,
                Object_Subtype => Object_Subtype,
                Renames_Token => Renames_Token,
                Renamed_Object => Renamed_Object, With_Token => With_Token,
                Aspects => Aspects, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Object_Renaming_Declarations
        .Object_Renaming_Declaration_Access
        (Result);
   end Create_Object_Renaming_Declaration;

   not overriding function Create_Exception_Renaming_Declaration
    (Self : Element_Factory;
     Names             : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Exception_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Renames_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Renamed_Exception : not null Program.Elements.Expressions
         .Expression_Access;
     With_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects           : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Exception_Renaming_Declarations
          .Exception_Renaming_Declaration_Access is
      Result : constant Exception_Renaming_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Exception_Renaming_Declarations
            .Exception_Renaming_Declaration'
            (Program.Nodes.Exception_Renaming_Declarations.Create
               (Names => Names, Colon_Token => Colon_Token,
                Exception_Token => Exception_Token,
                Renames_Token => Renames_Token,
                Renamed_Exception => Renamed_Exception,
                With_Token => With_Token, Aspects => Aspects,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Exception_Renaming_Declarations
        .Exception_Renaming_Declaration_Access
        (Result);
   end Create_Exception_Renaming_Declaration;

   not overriding function Create_Procedure_Renaming_Declaration
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Renames_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Renamed_Procedure   : Program.Elements.Expressions.Expression_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Procedure_Renaming_Declarations
          .Procedure_Renaming_Declaration_Access is
      Result : constant Procedure_Renaming_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Procedure_Renaming_Declarations
            .Procedure_Renaming_Declaration'
            (Program.Nodes.Procedure_Renaming_Declarations.Create
               (Not_Token => Not_Token, Overriding_Token => Overriding_Token,
                Procedure_Token => Procedure_Token, Name => Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token,
                Renames_Token => Renames_Token,
                Renamed_Procedure => Renamed_Procedure,
                With_Token => With_Token, Aspects => Aspects,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Procedure_Renaming_Declarations
        .Procedure_Renaming_Declaration_Access
        (Result);
   end Create_Procedure_Renaming_Declaration;

   not overriding function Create_Function_Renaming_Declaration
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token_2         : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Subtype      : not null Program.Elements.Element_Access;
     Renames_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Renamed_Function    : Program.Elements.Expressions.Expression_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Function_Renaming_Declarations
          .Function_Renaming_Declaration_Access is
      Result : constant Function_Renaming_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Function_Renaming_Declarations
            .Function_Renaming_Declaration'
            (Program.Nodes.Function_Renaming_Declarations.Create
               (Not_Token => Not_Token, Overriding_Token => Overriding_Token,
                Function_Token => Function_Token, Name => Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token,
                Return_Token => Return_Token, Not_Token_2 => Not_Token_2,
                Null_Token => Null_Token, Result_Subtype => Result_Subtype,
                Renames_Token => Renames_Token,
                Renamed_Function => Renamed_Function, With_Token => With_Token,
                Aspects => Aspects, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Function_Renaming_Declarations
        .Function_Renaming_Declaration_Access
        (Result);
   end Create_Function_Renaming_Declaration;

   not overriding function Create_Package_Renaming_Declaration
    (Self : Element_Factory;
     Package_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Renames_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Renamed_Package : not null Program.Elements.Expressions.Expression_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Package_Renaming_Declarations
          .Package_Renaming_Declaration_Access is
      Result : constant Package_Renaming_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Package_Renaming_Declarations
            .Package_Renaming_Declaration'
            (Program.Nodes.Package_Renaming_Declarations.Create
               (Package_Token => Package_Token, Name => Name,
                Renames_Token => Renames_Token,
                Renamed_Package => Renamed_Package, With_Token => With_Token,
                Aspects => Aspects, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Package_Renaming_Declarations
        .Package_Renaming_Declaration_Access
        (Result);
   end Create_Package_Renaming_Declaration;

   not overriding function Create_Generic_Package_Renaming_Declaration
    (Self : Element_Factory;
     Generic_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Package_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Renames_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Renamed_Package : not null Program.Elements.Expressions.Expression_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Generic_Package_Renaming_Declarations
          .Generic_Package_Renaming_Declaration_Access is
      Result : constant Generic_Package_Renaming_Declaration_Access :=

          new (Self.Subpool) Program.Nodes
            .Generic_Package_Renaming_Declarations
            .Generic_Package_Renaming_Declaration'
            (Program.Nodes.Generic_Package_Renaming_Declarations.Create
               (Generic_Token => Generic_Token, Package_Token => Package_Token,
                Name => Name, Renames_Token => Renames_Token,
                Renamed_Package => Renamed_Package, With_Token => With_Token,
                Aspects => Aspects, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Generic_Package_Renaming_Declarations
        .Generic_Package_Renaming_Declaration_Access
        (Result);
   end Create_Generic_Package_Renaming_Declaration;

   not overriding function Create_Generic_Procedure_Renaming_Declaration
    (Self : Element_Factory;
     Generic_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Procedure_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name              : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Renames_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Renamed_Procedure : not null Program.Elements.Expressions
         .Expression_Access;
     With_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects           : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Generic_Procedure_Renaming_Declarations
          .Generic_Procedure_Renaming_Declaration_Access is
      Result : constant Generic_Procedure_Renaming_Declaration_Access :=

          new (Self.Subpool) Program.Nodes
            .Generic_Procedure_Renaming_Declarations
            .Generic_Procedure_Renaming_Declaration'
            (Program.Nodes.Generic_Procedure_Renaming_Declarations.Create
               (Generic_Token => Generic_Token,
                Procedure_Token => Procedure_Token, Name => Name,
                Renames_Token => Renames_Token,
                Renamed_Procedure => Renamed_Procedure,
                With_Token => With_Token, Aspects => Aspects,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Generic_Procedure_Renaming_Declarations
        .Generic_Procedure_Renaming_Declaration_Access
        (Result);
   end Create_Generic_Procedure_Renaming_Declaration;

   not overriding function Create_Generic_Function_Renaming_Declaration
    (Self : Element_Factory;
     Generic_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Function_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name             : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Renames_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Renamed_Function : not null Program.Elements.Expressions
         .Expression_Access;
     With_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects          : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Generic_Function_Renaming_Declarations
          .Generic_Function_Renaming_Declaration_Access is
      Result : constant Generic_Function_Renaming_Declaration_Access :=

          new (Self.Subpool) Program.Nodes
            .Generic_Function_Renaming_Declarations
            .Generic_Function_Renaming_Declaration'
            (Program.Nodes.Generic_Function_Renaming_Declarations.Create
               (Generic_Token => Generic_Token,
                Function_Token => Function_Token, Name => Name,
                Renames_Token => Renames_Token,
                Renamed_Function => Renamed_Function, With_Token => With_Token,
                Aspects => Aspects, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Generic_Function_Renaming_Declarations
        .Generic_Function_Renaming_Declaration_Access
        (Result);
   end Create_Generic_Function_Renaming_Declaration;

   not overriding function Create_Task_Body_Declaration
    (Self : Element_Factory;
     Task_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Body_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name               : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects            : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Declarations       : not null Program.Element_Vectors
         .Element_Vector_Access;
     Begin_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements         : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Exception_Handlers : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name           : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Task_Body_Declarations
          .Task_Body_Declaration_Access is
      Result : constant Task_Body_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Task_Body_Declarations
            .Task_Body_Declaration'
            (Program.Nodes.Task_Body_Declarations.Create
               (Task_Token => Task_Token, Body_Token => Body_Token,
                Name => Name, With_Token => With_Token, Aspects => Aspects,
                Is_Token => Is_Token, Declarations => Declarations,
                Begin_Token => Begin_Token, Statements => Statements,
                Exception_Token => Exception_Token,
                Exception_Handlers => Exception_Handlers,
                End_Token => End_Token, End_Name => End_Name,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Task_Body_Declarations
        .Task_Body_Declaration_Access
        (Result);
   end Create_Task_Body_Declaration;

   not overriding function Create_Protected_Body_Declaration
    (Self : Element_Factory;
     Protected_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Body_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     With_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Protected_Operations : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Protected_Body_Declarations
          .Protected_Body_Declaration_Access is
      Result : constant Protected_Body_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Protected_Body_Declarations
            .Protected_Body_Declaration'
            (Program.Nodes.Protected_Body_Declarations.Create
               (Protected_Token => Protected_Token, Body_Token => Body_Token,
                Name => Name, With_Token => With_Token, Aspects => Aspects,
                Is_Token => Is_Token,
                Protected_Operations => Protected_Operations,
                End_Token => End_Token, End_Name => End_Name,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Protected_Body_Declarations
        .Protected_Body_Declaration_Access
        (Result);
   end Create_Protected_Body_Declaration;

   not overriding function Create_Entry_Declaration
    (Self : Element_Factory;
     Not_Token               : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Entry_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                    : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Left_Bracket_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Entry_Family_Definition : Program.Elements.Discrete_Subtype_Definitions
         .Discrete_Subtype_Definition_Access;
     Right_Bracket_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Left_Bracket_Token_2    : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters              : not null Program.Elements
         .Parameter_Specifications.Parameter_Specification_Vector_Access;
     Right_Bracket_Token_2   : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token              : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects                 : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Entry_Declarations
          .Entry_Declaration_Access is
      Result : constant Entry_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Entry_Declarations
            .Entry_Declaration'
            (Program.Nodes.Entry_Declarations.Create
               (Not_Token => Not_Token, Overriding_Token => Overriding_Token,
                Entry_Token => Entry_Token, Name => Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Entry_Family_Definition => Entry_Family_Definition,
                Right_Bracket_Token => Right_Bracket_Token,
                Left_Bracket_Token_2 => Left_Bracket_Token_2,
                Parameters => Parameters,
                Right_Bracket_Token_2 => Right_Bracket_Token_2,
                With_Token => With_Token, Aspects => Aspects,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Entry_Declarations.Entry_Declaration_Access
        (Result);
   end Create_Entry_Declaration;

   not overriding function Create_Entry_Body_Declaration
    (Self : Element_Factory;
     Entry_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                  : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Left_Bracket_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Entry_Index           : not null Program.Elements
         .Entry_Index_Specifications.Entry_Index_Specification_Access;
     Right_Bracket_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Left_Bracket_Token_2  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters            : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token_2 : Program.Lexical_Elements.Lexical_Element_Access;
     When_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Entry_Barrier         : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Token              : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Declarations          : not null Program.Element_Vectors
         .Element_Vector_Access;
     Begin_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements            : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Exception_Handlers    : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name              : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Entry_Body_Declarations
          .Entry_Body_Declaration_Access is
      Result : constant Entry_Body_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Entry_Body_Declarations
            .Entry_Body_Declaration'
            (Program.Nodes.Entry_Body_Declarations.Create
               (Entry_Token => Entry_Token, Name => Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Entry_Index => Entry_Index,
                Right_Bracket_Token => Right_Bracket_Token,
                Left_Bracket_Token_2 => Left_Bracket_Token_2,
                Parameters => Parameters,
                Right_Bracket_Token_2 => Right_Bracket_Token_2,
                When_Token => When_Token, Entry_Barrier => Entry_Barrier,
                Is_Token => Is_Token, Declarations => Declarations,
                Begin_Token => Begin_Token, Statements => Statements,
                Exception_Token => Exception_Token,
                Exception_Handlers => Exception_Handlers,
                End_Token => End_Token, End_Name => End_Name,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Entry_Body_Declarations
        .Entry_Body_Declaration_Access
        (Result);
   end Create_Entry_Body_Declaration;

   not overriding function Create_Entry_Index_Specification
    (Self : Element_Factory;
     For_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     In_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Entry_Index_Subtype : not null Program.Elements
         .Discrete_Subtype_Definitions.Discrete_Subtype_Definition_Access)
      return not null Program.Elements.Entry_Index_Specifications
          .Entry_Index_Specification_Access is
      Result : constant Entry_Index_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Entry_Index_Specifications
            .Entry_Index_Specification'
            (Program.Nodes.Entry_Index_Specifications.Create
               (For_Token => For_Token, Name => Name, In_Token => In_Token,
                Entry_Index_Subtype => Entry_Index_Subtype));
   begin
      return Program.Elements.Entry_Index_Specifications
        .Entry_Index_Specification_Access
        (Result);
   end Create_Entry_Index_Specification;

   not overriding function Create_Procedure_Body_Stub
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Is_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Separate_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Procedure_Body_Stubs
          .Procedure_Body_Stub_Access is
      Result : constant Procedure_Body_Stub_Access :=

          new (Self.Subpool) Program.Nodes.Procedure_Body_Stubs
            .Procedure_Body_Stub'
            (Program.Nodes.Procedure_Body_Stubs.Create
               (Not_Token => Not_Token, Overriding_Token => Overriding_Token,
                Procedure_Token => Procedure_Token, Name => Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token,
                Is_Token => Is_Token, Separate_Token => Separate_Token,
                With_Token => With_Token, Aspects => Aspects,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Procedure_Body_Stubs.Procedure_Body_Stub_Access
        (Result);
   end Create_Procedure_Body_Stub;

   not overriding function Create_Function_Body_Stub
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token_2         : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Subtype      : not null Program.Elements.Element_Access;
     Is_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Separate_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Function_Body_Stubs
          .Function_Body_Stub_Access is
      Result : constant Function_Body_Stub_Access :=

          new (Self.Subpool) Program.Nodes.Function_Body_Stubs
            .Function_Body_Stub'
            (Program.Nodes.Function_Body_Stubs.Create
               (Not_Token => Not_Token, Overriding_Token => Overriding_Token,
                Function_Token => Function_Token, Name => Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token,
                Return_Token => Return_Token, Not_Token_2 => Not_Token_2,
                Null_Token => Null_Token, Result_Subtype => Result_Subtype,
                Is_Token => Is_Token, Separate_Token => Separate_Token,
                With_Token => With_Token, Aspects => Aspects,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Function_Body_Stubs.Function_Body_Stub_Access
        (Result);
   end Create_Function_Body_Stub;

   not overriding function Create_Package_Body_Stub
    (Self : Element_Factory;
     Package_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Body_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Separate_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Package_Body_Stubs
          .Package_Body_Stub_Access is
      Result : constant Package_Body_Stub_Access :=

          new (Self.Subpool) Program.Nodes.Package_Body_Stubs
            .Package_Body_Stub'
            (Program.Nodes.Package_Body_Stubs.Create
               (Package_Token => Package_Token, Body_Token => Body_Token,
                Name => Name, Is_Token => Is_Token,
                Separate_Token => Separate_Token, With_Token => With_Token,
                Aspects => Aspects, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Package_Body_Stubs.Package_Body_Stub_Access
        (Result);
   end Create_Package_Body_Stub;

   not overriding function Create_Task_Body_Stub
    (Self : Element_Factory;
     Task_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Body_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Separate_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Task_Body_Stubs.Task_Body_Stub_Access is
      Result : constant Task_Body_Stub_Access :=

          new (Self.Subpool) Program.Nodes.Task_Body_Stubs.Task_Body_Stub'
            (Program.Nodes.Task_Body_Stubs.Create
               (Task_Token => Task_Token, Body_Token => Body_Token,
                Name => Name, Is_Token => Is_Token,
                Separate_Token => Separate_Token, With_Token => With_Token,
                Aspects => Aspects, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Task_Body_Stubs.Task_Body_Stub_Access (Result);
   end Create_Task_Body_Stub;

   not overriding function Create_Protected_Body_Stub
    (Self : Element_Factory;
     Protected_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Body_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Separate_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Protected_Body_Stubs
          .Protected_Body_Stub_Access is
      Result : constant Protected_Body_Stub_Access :=

          new (Self.Subpool) Program.Nodes.Protected_Body_Stubs
            .Protected_Body_Stub'
            (Program.Nodes.Protected_Body_Stubs.Create
               (Protected_Token => Protected_Token, Body_Token => Body_Token,
                Name => Name, Is_Token => Is_Token,
                Separate_Token => Separate_Token, With_Token => With_Token,
                Aspects => Aspects, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Protected_Body_Stubs.Protected_Body_Stub_Access
        (Result);
   end Create_Protected_Body_Stub;

   not overriding function Create_Exception_Declaration
    (Self : Element_Factory;
     Names           : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Exception_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Exception_Declarations
          .Exception_Declaration_Access is
      Result : constant Exception_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Exception_Declarations
            .Exception_Declaration'
            (Program.Nodes.Exception_Declarations.Create
               (Names => Names, Colon_Token => Colon_Token,
                Exception_Token => Exception_Token, With_Token => With_Token,
                Aspects => Aspects, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Exception_Declarations
        .Exception_Declaration_Access
        (Result);
   end Create_Exception_Declaration;

   not overriding function Create_Choice_Parameter_Specification
    (Self : Element_Factory;
     Name        : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Choice_Parameter_Specifications
          .Choice_Parameter_Specification_Access is
      Result : constant Choice_Parameter_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Choice_Parameter_Specifications
            .Choice_Parameter_Specification'
            (Program.Nodes.Choice_Parameter_Specifications.Create
               (Name => Name, Colon_Token => Colon_Token));
   begin
      return Program.Elements.Choice_Parameter_Specifications
        .Choice_Parameter_Specification_Access
        (Result);
   end Create_Choice_Parameter_Specification;

   not overriding function Create_Generic_Package_Declaration
    (Self : Element_Factory;
     Generic_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Formal_Parameters    : not null Program.Element_Vectors
         .Element_Vector_Access;
     Package_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     With_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Visible_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     Private_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name             : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Generic_Package_Declarations
          .Generic_Package_Declaration_Access is
      Result : constant Generic_Package_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Generic_Package_Declarations
            .Generic_Package_Declaration'
            (Program.Nodes.Generic_Package_Declarations.Create
               (Generic_Token => Generic_Token,
                Formal_Parameters => Formal_Parameters,
                Package_Token => Package_Token, Name => Name,
                With_Token => With_Token, Aspects => Aspects,
                Is_Token => Is_Token,
                Visible_Declarations => Visible_Declarations,
                Private_Token => Private_Token,
                Private_Declarations => Private_Declarations,
                End_Token => End_Token, End_Name => End_Name,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Generic_Package_Declarations
        .Generic_Package_Declaration_Access
        (Result);
   end Create_Generic_Package_Declaration;

   not overriding function Create_Generic_Procedure_Declaration
    (Self : Element_Factory;
     Generic_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Formal_Parameters   : not null Program.Element_Vectors
         .Element_Vector_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Generic_Procedure_Declarations
          .Generic_Procedure_Declaration_Access is
      Result : constant Generic_Procedure_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Generic_Procedure_Declarations
            .Generic_Procedure_Declaration'
            (Program.Nodes.Generic_Procedure_Declarations.Create
               (Generic_Token => Generic_Token,
                Formal_Parameters => Formal_Parameters,
                Procedure_Token => Procedure_Token, Name => Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token,
                With_Token => With_Token, Aspects => Aspects,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Generic_Procedure_Declarations
        .Generic_Procedure_Declaration_Access
        (Result);
   end Create_Generic_Procedure_Declaration;

   not overriding function Create_Generic_Function_Declaration
    (Self : Element_Factory;
     Generic_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Formal_Parameters   : not null Program.Element_Vectors
         .Element_Vector_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Subtype      : not null Program.Elements.Element_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Generic_Function_Declarations
          .Generic_Function_Declaration_Access is
      Result : constant Generic_Function_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Generic_Function_Declarations
            .Generic_Function_Declaration'
            (Program.Nodes.Generic_Function_Declarations.Create
               (Generic_Token => Generic_Token,
                Formal_Parameters => Formal_Parameters,
                Function_Token => Function_Token, Name => Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token,
                Return_Token => Return_Token, Not_Token => Not_Token,
                Null_Token => Null_Token, Result_Subtype => Result_Subtype,
                With_Token => With_Token, Aspects => Aspects,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Generic_Function_Declarations
        .Generic_Function_Declaration_Access
        (Result);
   end Create_Generic_Function_Declaration;

   not overriding function Create_Package_Instantiation
    (Self : Element_Factory;
     Package_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Generic_Package_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters           : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Package_Instantiations
          .Package_Instantiation_Access is
      Result : constant Package_Instantiation_Access :=

          new (Self.Subpool) Program.Nodes.Package_Instantiations
            .Package_Instantiation'
            (Program.Nodes.Package_Instantiations.Create
               (Package_Token => Package_Token, Name => Name,
                Is_Token => Is_Token, New_Token => New_Token,
                Generic_Package_Name => Generic_Package_Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token,
                With_Token => With_Token, Aspects => Aspects,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Package_Instantiations
        .Package_Instantiation_Access
        (Result);
   end Create_Package_Instantiation;

   not overriding function Create_Procedure_Instantiation
    (Self : Element_Factory;
     Not_Token              : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Procedure_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                   : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Is_Token               : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token              : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Generic_Procedure_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters             : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token             : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects                : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Procedure_Instantiations
          .Procedure_Instantiation_Access is
      Result : constant Procedure_Instantiation_Access :=

          new (Self.Subpool) Program.Nodes.Procedure_Instantiations
            .Procedure_Instantiation'
            (Program.Nodes.Procedure_Instantiations.Create
               (Not_Token => Not_Token, Overriding_Token => Overriding_Token,
                Procedure_Token => Procedure_Token, Name => Name,
                Is_Token => Is_Token, New_Token => New_Token,
                Generic_Procedure_Name => Generic_Procedure_Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token,
                With_Token => With_Token, Aspects => Aspects,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Procedure_Instantiations
        .Procedure_Instantiation_Access
        (Result);
   end Create_Procedure_Instantiation;

   not overriding function Create_Function_Instantiation
    (Self : Element_Factory;
     Not_Token             : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Function_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                  : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Is_Token              : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Generic_Function_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters            : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects               : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Function_Instantiations
          .Function_Instantiation_Access is
      Result : constant Function_Instantiation_Access :=

          new (Self.Subpool) Program.Nodes.Function_Instantiations
            .Function_Instantiation'
            (Program.Nodes.Function_Instantiations.Create
               (Not_Token => Not_Token, Overriding_Token => Overriding_Token,
                Function_Token => Function_Token, Name => Name,
                Is_Token => Is_Token, New_Token => New_Token,
                Generic_Function_Name => Generic_Function_Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token,
                With_Token => With_Token, Aspects => Aspects,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Function_Instantiations
        .Function_Instantiation_Access
        (Result);
   end Create_Function_Instantiation;

   not overriding function Create_Formal_Object_Declaration
    (Self : Element_Factory;
     Names              : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     In_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Out_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Not_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Object_Subtype     : not null Program.Elements.Element_Access;
     Assignment_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Default_Expression : Program.Elements.Expressions.Expression_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects            : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Formal_Object_Declarations
          .Formal_Object_Declaration_Access is
      Result : constant Formal_Object_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Object_Declarations
            .Formal_Object_Declaration'
            (Program.Nodes.Formal_Object_Declarations.Create
               (Names => Names, Colon_Token => Colon_Token,
                In_Token => In_Token, Out_Token => Out_Token,
                Not_Token => Not_Token, Null_Token => Null_Token,
                Object_Subtype => Object_Subtype,
                Assignment_Token => Assignment_Token,
                Default_Expression => Default_Expression,
                With_Token => With_Token, Aspects => Aspects,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Formal_Object_Declarations
        .Formal_Object_Declaration_Access
        (Result);
   end Create_Formal_Object_Declaration;

   not overriding function Create_Formal_Type_Declaration
    (Self : Element_Factory;
     Type_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name              : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Discriminant_Part : Program.Elements.Definitions.Definition_Access;
     Is_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Definition        : not null Program.Elements.Formal_Type_Definitions
         .Formal_Type_Definition_Access;
     With_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects           : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Formal_Type_Declarations
          .Formal_Type_Declaration_Access is
      Result : constant Formal_Type_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Type_Declarations
            .Formal_Type_Declaration'
            (Program.Nodes.Formal_Type_Declarations.Create
               (Type_Token => Type_Token, Name => Name,
                Discriminant_Part => Discriminant_Part, Is_Token => Is_Token,
                Definition => Definition, With_Token => With_Token,
                Aspects => Aspects, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Formal_Type_Declarations
        .Formal_Type_Declaration_Access
        (Result);
   end Create_Formal_Type_Declaration;

   not overriding function Create_Formal_Procedure_Declaration
    (Self : Element_Factory;
     With_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Is_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Abstract_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Subprogram_Default  : Program.Elements.Expressions.Expression_Access;
     Box_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token_2        : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Formal_Procedure_Declarations
          .Formal_Procedure_Declaration_Access is
      Result : constant Formal_Procedure_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Procedure_Declarations
            .Formal_Procedure_Declaration'
            (Program.Nodes.Formal_Procedure_Declarations.Create
               (With_Token => With_Token, Procedure_Token => Procedure_Token,
                Name => Name, Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token,
                Is_Token => Is_Token, Abstract_Token => Abstract_Token,
                Null_Token => Null_Token,
                Subprogram_Default => Subprogram_Default,
                Box_Token => Box_Token, With_Token_2 => With_Token_2,
                Aspects => Aspects, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Formal_Procedure_Declarations
        .Formal_Procedure_Declaration_Access
        (Result);
   end Create_Formal_Procedure_Declaration;

   not overriding function Create_Formal_Function_Declaration
    (Self : Element_Factory;
     With_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Subtype      : not null Program.Elements.Element_Access;
     Is_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Abstract_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Subprogram_Default  : Program.Elements.Expressions.Expression_Access;
     Box_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token_2        : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Formal_Function_Declarations
          .Formal_Function_Declaration_Access is
      Result : constant Formal_Function_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Function_Declarations
            .Formal_Function_Declaration'
            (Program.Nodes.Formal_Function_Declarations.Create
               (With_Token => With_Token, Function_Token => Function_Token,
                Name => Name, Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token,
                Return_Token => Return_Token, Not_Token => Not_Token,
                Null_Token => Null_Token, Result_Subtype => Result_Subtype,
                Is_Token => Is_Token, Abstract_Token => Abstract_Token,
                Subprogram_Default => Subprogram_Default,
                Box_Token => Box_Token, With_Token_2 => With_Token_2,
                Aspects => Aspects, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Formal_Function_Declarations
        .Formal_Function_Declaration_Access
        (Result);
   end Create_Formal_Function_Declaration;

   not overriding function Create_Formal_Package_Declaration
    (Self : Element_Factory;
     With_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Package_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Generic_Package_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters           : not null Program.Elements
         .Formal_Package_Associations.Formal_Package_Association_Vector_Access;
     Right_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token_2         : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Formal_Package_Declarations
          .Formal_Package_Declaration_Access is
      Result : constant Formal_Package_Declaration_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Package_Declarations
            .Formal_Package_Declaration'
            (Program.Nodes.Formal_Package_Declarations.Create
               (With_Token => With_Token, Package_Token => Package_Token,
                Name => Name, Is_Token => Is_Token, New_Token => New_Token,
                Generic_Package_Name => Generic_Package_Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token,
                With_Token_2 => With_Token_2, Aspects => Aspects,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Formal_Package_Declarations
        .Formal_Package_Declaration_Access
        (Result);
   end Create_Formal_Package_Declaration;

   not overriding function Create_Subtype_Indication
    (Self : Element_Factory;
     Not_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Subtype_Mark : not null Program.Elements.Expressions.Expression_Access;
     Constraint   : Program.Elements.Constraints.Constraint_Access)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access is
      Result : constant Subtype_Indication_Access :=

          new (Self.Subpool) Program.Nodes.Subtype_Indications
            .Subtype_Indication'
            (Program.Nodes.Subtype_Indications.Create
               (Not_Token => Not_Token, Null_Token => Null_Token,
                Subtype_Mark => Subtype_Mark, Constraint => Constraint));
   begin
      return Program.Elements.Subtype_Indications.Subtype_Indication_Access
        (Result);
   end Create_Subtype_Indication;

   not overriding function Create_Component_Definition
    (Self : Element_Factory;
     Aliased_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Subtype_Indication : not null Program.Elements.Element_Access)
      return not null Program.Elements.Component_Definitions
          .Component_Definition_Access is
      Result : constant Component_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Component_Definitions
            .Component_Definition'
            (Program.Nodes.Component_Definitions.Create
               (Aliased_Token => Aliased_Token,
                Subtype_Indication => Subtype_Indication));
   begin
      return Program.Elements.Component_Definitions.Component_Definition_Access
        (Result);
   end Create_Component_Definition;

   not overriding function Create_Unknown_Discriminant_Part
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Box_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Unknown_Discriminant_Parts
          .Unknown_Discriminant_Part_Access is
      Result : constant Unknown_Discriminant_Part_Access :=

          new (Self.Subpool) Program.Nodes.Unknown_Discriminant_Parts
            .Unknown_Discriminant_Part'
            (Program.Nodes.Unknown_Discriminant_Parts.Create
               (Left_Bracket_Token => Left_Bracket_Token,
                Box_Token => Box_Token,
                Right_Bracket_Token => Right_Bracket_Token));
   begin
      return Program.Elements.Unknown_Discriminant_Parts
        .Unknown_Discriminant_Part_Access
        (Result);
   end Create_Unknown_Discriminant_Part;

   not overriding function Create_Known_Discriminant_Part
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Discriminants       : not null Program.Elements
         .Discriminant_Specifications.Discriminant_Specification_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Known_Discriminant_Parts
          .Known_Discriminant_Part_Access is
      Result : constant Known_Discriminant_Part_Access :=

          new (Self.Subpool) Program.Nodes.Known_Discriminant_Parts
            .Known_Discriminant_Part'
            (Program.Nodes.Known_Discriminant_Parts.Create
               (Left_Bracket_Token => Left_Bracket_Token,
                Discriminants => Discriminants,
                Right_Bracket_Token => Right_Bracket_Token));
   begin
      return Program.Elements.Known_Discriminant_Parts
        .Known_Discriminant_Part_Access
        (Result);
   end Create_Known_Discriminant_Part;

   not overriding function Create_Record_Definition
    (Self : Element_Factory;
     Record_Token   : not null Program.Lexical_Elements.Lexical_Element_Access;
     Components     : not null Program.Element_Vectors.Element_Vector_Access;
     End_Token      : not null Program.Lexical_Elements.Lexical_Element_Access;
     Record_Token_2 : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Record_Definitions
          .Record_Definition_Access is
      Result : constant Record_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Record_Definitions
            .Record_Definition'
            (Program.Nodes.Record_Definitions.Create
               (Record_Token => Record_Token, Components => Components,
                End_Token => End_Token, Record_Token_2 => Record_Token_2));
   begin
      return Program.Elements.Record_Definitions.Record_Definition_Access
        (Result);
   end Create_Record_Definition;

   not overriding function Create_Null_Component
    (Self : Element_Factory;
     Null_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Null_Components.Null_Component_Access is
      Result : constant Null_Component_Access :=

          new (Self.Subpool) Program.Nodes.Null_Components.Null_Component'
            (Program.Nodes.Null_Components.Create
               (Null_Token => Null_Token, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Null_Components.Null_Component_Access (Result);
   end Create_Null_Component;

   not overriding function Create_Variant_Part
    (Self : Element_Factory;
     Case_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Discriminant    : not null Program.Elements.Identifiers.Identifier_Access;
     Is_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Variants        : not null Program.Elements.Variants
         .Variant_Vector_Access;
     End_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Case_Token_2    : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Variant_Parts.Variant_Part_Access is
      Result : constant Variant_Part_Access :=

          new (Self.Subpool) Program.Nodes.Variant_Parts.Variant_Part'
            (Program.Nodes.Variant_Parts.Create
               (Case_Token => Case_Token, Discriminant => Discriminant,
                Is_Token => Is_Token, Variants => Variants,
                End_Token => End_Token, Case_Token_2 => Case_Token_2,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Variant_Parts.Variant_Part_Access (Result);
   end Create_Variant_Part;

   not overriding function Create_Variant
    (Self : Element_Factory;
     When_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Choices     : not null Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Components  : not null Program.Element_Vectors.Element_Vector_Access)
      return not null Program.Elements.Variants.Variant_Access is
      Result : constant Variant_Access :=

          new (Self.Subpool) Program.Nodes.Variants.Variant'
            (Program.Nodes.Variants.Create
               (When_Token => When_Token, Choices => Choices,
                Arrow_Token => Arrow_Token, Components => Components));
   begin
      return Program.Elements.Variants.Variant_Access (Result);
   end Create_Variant;

   not overriding function Create_Others_Choice
    (Self         : Element_Factory;
     Others_Token : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Others_Choices.Others_Choice_Access is
      Result : constant Others_Choice_Access :=

          new (Self.Subpool) Program.Nodes.Others_Choices.Others_Choice'
            (Program.Nodes.Others_Choices.Create
               (Others_Token => Others_Token));
   begin
      return Program.Elements.Others_Choices.Others_Choice_Access (Result);
   end Create_Others_Choice;

   not overriding function Create_Private_Type_Definition
    (Self : Element_Factory;
     Abstract_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Tagged_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Limited_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Token  : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Private_Type_Definitions
          .Private_Type_Definition_Access is
      Result : constant Private_Type_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Private_Type_Definitions
            .Private_Type_Definition'
            (Program.Nodes.Private_Type_Definitions.Create
               (Abstract_Token => Abstract_Token, Tagged_Token => Tagged_Token,
                Limited_Token => Limited_Token,
                Private_Token => Private_Token));
   begin
      return Program.Elements.Private_Type_Definitions
        .Private_Type_Definition_Access
        (Result);
   end Create_Private_Type_Definition;

   not overriding function Create_Private_Extension_Definition
    (Self : Element_Factory;
     Abstract_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Limited_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Synchronized_Token : Program.Lexical_Elements.Lexical_Element_Access;
     New_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Ancestor           : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     And_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors        : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     With_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Private_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Private_Extension_Definitions
          .Private_Extension_Definition_Access is
      Result : constant Private_Extension_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Private_Extension_Definitions
            .Private_Extension_Definition'
            (Program.Nodes.Private_Extension_Definitions.Create
               (Abstract_Token => Abstract_Token,
                Limited_Token => Limited_Token,
                Synchronized_Token => Synchronized_Token,
                New_Token => New_Token, Ancestor => Ancestor,
                And_Token => And_Token, Progenitors => Progenitors,
                With_Token => With_Token, Private_Token => Private_Token));
   begin
      return Program.Elements.Private_Extension_Definitions
        .Private_Extension_Definition_Access
        (Result);
   end Create_Private_Extension_Definition;

   not overriding function Create_Incomplete_Type_Definition
    (Self         : Element_Factory;
     Tagged_Token : Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Incomplete_Type_Definitions
          .Incomplete_Type_Definition_Access is
      Result : constant Incomplete_Type_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Incomplete_Type_Definitions
            .Incomplete_Type_Definition'
            (Program.Nodes.Incomplete_Type_Definitions.Create
               (Tagged_Token => Tagged_Token));
   begin
      return Program.Elements.Incomplete_Type_Definitions
        .Incomplete_Type_Definition_Access
        (Result);
   end Create_Incomplete_Type_Definition;

   not overriding function Create_Task_Definition
    (Self : Element_Factory;
     Visible_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     Private_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Private_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access)
      return not null Program.Elements.Task_Definitions
          .Task_Definition_Access is
      Result : constant Task_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Task_Definitions.Task_Definition'
            (Program.Nodes.Task_Definitions.Create
               (Visible_Declarations => Visible_Declarations,
                Private_Token => Private_Token,
                Private_Declarations => Private_Declarations,
                End_Token => End_Token, End_Name => End_Name));
   begin
      return Program.Elements.Task_Definitions.Task_Definition_Access (Result);
   end Create_Task_Definition;

   not overriding function Create_Protected_Definition
    (Self : Element_Factory;
     Visible_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     Private_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Private_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access)
      return not null Program.Elements.Protected_Definitions
          .Protected_Definition_Access is
      Result : constant Protected_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Protected_Definitions
            .Protected_Definition'
            (Program.Nodes.Protected_Definitions.Create
               (Visible_Declarations => Visible_Declarations,
                Private_Token => Private_Token,
                Private_Declarations => Private_Declarations,
                End_Token => End_Token, End_Name => End_Name));
   begin
      return Program.Elements.Protected_Definitions.Protected_Definition_Access
        (Result);
   end Create_Protected_Definition;

   not overriding function Create_Aspect_Specification
    (Self : Element_Factory;
     Aspect_Mark       : not null Program.Elements.Expressions
         .Expression_Access;
     Arrow_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Aspect_Definition : not null Program.Elements.Expressions
         .Expression_Access)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Access is
      Result : constant Aspect_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Aspect_Specifications
            .Aspect_Specification'
            (Program.Nodes.Aspect_Specifications.Create
               (Aspect_Mark => Aspect_Mark, Arrow_Token => Arrow_Token,
                Aspect_Definition => Aspect_Definition));
   begin
      return Program.Elements.Aspect_Specifications.Aspect_Specification_Access
        (Result);
   end Create_Aspect_Specification;

   not overriding function Create_Real_Range_Specification
    (Self : Element_Factory;
     Range_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Lower_Bound      : not null Program.Elements.Expressions
         .Expression_Access;
     Double_Dot_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Upper_Bound      : not null Program.Elements.Expressions
         .Expression_Access)
      return not null Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Access is
      Result : constant Real_Range_Specification_Access :=

          new (Self.Subpool) Program.Nodes.Real_Range_Specifications
            .Real_Range_Specification'
            (Program.Nodes.Real_Range_Specifications.Create
               (Range_Token => Range_Token, Lower_Bound => Lower_Bound,
                Double_Dot_Token => Double_Dot_Token,
                Upper_Bound => Upper_Bound));
   begin
      return Program.Elements.Real_Range_Specifications
        .Real_Range_Specification_Access
        (Result);
   end Create_Real_Range_Specification;

   not overriding function Create_Numeric_Literal
    (Self                  : Element_Factory;
     Numeric_Literal_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Numeric_Literals
          .Numeric_Literal_Access is
      Result : constant Numeric_Literal_Access :=

          new (Self.Subpool) Program.Nodes.Numeric_Literals.Numeric_Literal'
            (Program.Nodes.Numeric_Literals.Create
               (Numeric_Literal_Token => Numeric_Literal_Token));
   begin
      return Program.Elements.Numeric_Literals.Numeric_Literal_Access (Result);
   end Create_Numeric_Literal;

   not overriding function Create_String_Literal
    (Self                 : Element_Factory;
     String_Literal_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.String_Literals.String_Literal_Access is
      Result : constant String_Literal_Access :=

          new (Self.Subpool) Program.Nodes.String_Literals.String_Literal'
            (Program.Nodes.String_Literals.Create
               (String_Literal_Token => String_Literal_Token));
   begin
      return Program.Elements.String_Literals.String_Literal_Access (Result);
   end Create_String_Literal;

   not overriding function Create_Identifier
    (Self             : Element_Factory;
     Identifier_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Identifiers.Identifier_Access is
      Result : constant Identifier_Access :=

          new (Self.Subpool) Program.Nodes.Identifiers.Identifier'
            (Program.Nodes.Identifiers.Create
               (Identifier_Token => Identifier_Token));
   begin
      return Program.Elements.Identifiers.Identifier_Access (Result);
   end Create_Identifier;

   not overriding function Create_Operator_Symbol
    (Self                  : Element_Factory;
     Operator_Symbol_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Operator_Symbols
          .Operator_Symbol_Access is
      Result : constant Operator_Symbol_Access :=

          new (Self.Subpool) Program.Nodes.Operator_Symbols.Operator_Symbol'
            (Program.Nodes.Operator_Symbols.Create
               (Operator_Symbol_Token => Operator_Symbol_Token));
   begin
      return Program.Elements.Operator_Symbols.Operator_Symbol_Access (Result);
   end Create_Operator_Symbol;

   not overriding function Create_Character_Literal
    (Self                    : Element_Factory;
     Character_Literal_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Character_Literals
          .Character_Literal_Access is
      Result : constant Character_Literal_Access :=

          new (Self.Subpool) Program.Nodes.Character_Literals
            .Character_Literal'
            (Program.Nodes.Character_Literals.Create
               (Character_Literal_Token => Character_Literal_Token));
   begin
      return Program.Elements.Character_Literals.Character_Literal_Access
        (Result);
   end Create_Character_Literal;

   not overriding function Create_Explicit_Dereference
    (Self : Element_Factory;
     Prefix    : not null Program.Elements.Expressions.Expression_Access;
     Dot_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     All_Token : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Explicit_Dereferences
          .Explicit_Dereference_Access is
      Result : constant Explicit_Dereference_Access :=

          new (Self.Subpool) Program.Nodes.Explicit_Dereferences
            .Explicit_Dereference'
            (Program.Nodes.Explicit_Dereferences.Create
               (Prefix => Prefix, Dot_Token => Dot_Token,
                All_Token => All_Token));
   begin
      return Program.Elements.Explicit_Dereferences.Explicit_Dereference_Access
        (Result);
   end Create_Explicit_Dereference;

   not overriding function Create_Function_Call
    (Self : Element_Factory;
     Prefix              : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Function_Calls.Function_Call_Access is
      Result : constant Function_Call_Access :=

          new (Self.Subpool) Program.Nodes.Function_Calls.Function_Call'
            (Program.Nodes.Function_Calls.Create
               (Prefix => Prefix, Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token));
   begin
      return Program.Elements.Function_Calls.Function_Call_Access (Result);
   end Create_Function_Call;

   not overriding function Create_Indexed_Component
    (Self : Element_Factory;
     Prefix              : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expressions         : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Indexed_Components
          .Indexed_Component_Access is
      Result : constant Indexed_Component_Access :=

          new (Self.Subpool) Program.Nodes.Indexed_Components
            .Indexed_Component'
            (Program.Nodes.Indexed_Components.Create
               (Prefix => Prefix, Left_Bracket_Token => Left_Bracket_Token,
                Expressions => Expressions,
                Right_Bracket_Token => Right_Bracket_Token));
   begin
      return Program.Elements.Indexed_Components.Indexed_Component_Access
        (Result);
   end Create_Indexed_Component;

   not overriding function Create_Slice
    (Self : Element_Factory;
     Prefix              : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Slice_Range         : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Slices.Slice_Access is
      Result : constant Slice_Access :=

          new (Self.Subpool) Program.Nodes.Slices.Slice'
            (Program.Nodes.Slices.Create
               (Prefix => Prefix, Left_Bracket_Token => Left_Bracket_Token,
                Slice_Range => Slice_Range,
                Right_Bracket_Token => Right_Bracket_Token));
   begin
      return Program.Elements.Slices.Slice_Access (Result);
   end Create_Slice;

   not overriding function Create_Selected_Component
    (Self : Element_Factory;
     Prefix    : not null Program.Elements.Expressions.Expression_Access;
     Dot_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Selector  : not null Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Selected_Components
          .Selected_Component_Access is
      Result : constant Selected_Component_Access :=

          new (Self.Subpool) Program.Nodes.Selected_Components
            .Selected_Component'
            (Program.Nodes.Selected_Components.Create
               (Prefix => Prefix, Dot_Token => Dot_Token,
                Selector => Selector));
   begin
      return Program.Elements.Selected_Components.Selected_Component_Access
        (Result);
   end Create_Selected_Component;

   not overriding function Create_Attribute_Reference
    (Self : Element_Factory;
     Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Apostrophe_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Attribute_Designator : not null Program.Elements.Identifiers
         .Identifier_Access;
     Left_Bracket_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expressions          : Program.Elements.Expressions.Expression_Access;
     Right_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Attribute_References
          .Attribute_Reference_Access is
      Result : constant Attribute_Reference_Access :=

          new (Self.Subpool) Program.Nodes.Attribute_References
            .Attribute_Reference'
            (Program.Nodes.Attribute_References.Create
               (Prefix => Prefix, Apostrophe_Token => Apostrophe_Token,
                Attribute_Designator => Attribute_Designator,
                Left_Bracket_Token => Left_Bracket_Token,
                Expressions => Expressions,
                Right_Bracket_Token => Right_Bracket_Token));
   begin
      return Program.Elements.Attribute_References.Attribute_Reference_Access
        (Result);
   end Create_Attribute_Reference;

   not overriding function Create_Record_Aggregate
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Components          : not null Program.Elements
         .Record_Component_Associations
         .Record_Component_Association_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Record_Aggregates
          .Record_Aggregate_Access is
      Result : constant Record_Aggregate_Access :=

          new (Self.Subpool) Program.Nodes.Record_Aggregates.Record_Aggregate'
            (Program.Nodes.Record_Aggregates.Create
               (Left_Bracket_Token => Left_Bracket_Token,
                Components => Components,
                Right_Bracket_Token => Right_Bracket_Token));
   begin
      return Program.Elements.Record_Aggregates.Record_Aggregate_Access
        (Result);
   end Create_Record_Aggregate;

   not overriding function Create_Extension_Aggregate
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Ancestor            : not null Program.Elements.Expressions
         .Expression_Access;
     With_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Components          : not null Program.Elements
         .Record_Component_Associations
         .Record_Component_Association_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Extension_Aggregates
          .Extension_Aggregate_Access is
      Result : constant Extension_Aggregate_Access :=

          new (Self.Subpool) Program.Nodes.Extension_Aggregates
            .Extension_Aggregate'
            (Program.Nodes.Extension_Aggregates.Create
               (Left_Bracket_Token => Left_Bracket_Token, Ancestor => Ancestor,
                With_Token => With_Token, Components => Components,
                Right_Bracket_Token => Right_Bracket_Token));
   begin
      return Program.Elements.Extension_Aggregates.Extension_Aggregate_Access
        (Result);
   end Create_Extension_Aggregate;

   not overriding function Create_Array_Aggregate
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Components          : not null Program.Elements
         .Array_Component_Associations
         .Array_Component_Association_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Array_Aggregates
          .Array_Aggregate_Access is
      Result : constant Array_Aggregate_Access :=

          new (Self.Subpool) Program.Nodes.Array_Aggregates.Array_Aggregate'
            (Program.Nodes.Array_Aggregates.Create
               (Left_Bracket_Token => Left_Bracket_Token,
                Components => Components,
                Right_Bracket_Token => Right_Bracket_Token));
   begin
      return Program.Elements.Array_Aggregates.Array_Aggregate_Access (Result);
   end Create_Array_Aggregate;

   not overriding function Create_Short_Circuit_Operation
    (Self : Element_Factory;
     Left       : not null Program.Elements.Expressions.Expression_Access;
     And_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Then_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Or_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Else_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Right      : not null Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Short_Circuit_Operations
          .Short_Circuit_Operation_Access is
      Result : constant Short_Circuit_Operation_Access :=

          new (Self.Subpool) Program.Nodes.Short_Circuit_Operations
            .Short_Circuit_Operation'
            (Program.Nodes.Short_Circuit_Operations.Create
               (Left => Left, And_Token => And_Token, Then_Token => Then_Token,
                Or_Token => Or_Token, Else_Token => Else_Token,
                Right => Right));
   begin
      return Program.Elements.Short_Circuit_Operations
        .Short_Circuit_Operation_Access
        (Result);
   end Create_Short_Circuit_Operation;

   not overriding function Create_Membership_Test
    (Self : Element_Factory;
     Expression : not null Program.Elements.Expressions.Expression_Access;
     Not_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     In_Token   : not null Program.Lexical_Elements.Lexical_Element_Access;
     Choices    : not null Program.Element_Vectors.Element_Vector_Access)
      return not null Program.Elements.Membership_Tests
          .Membership_Test_Access is
      Result : constant Membership_Test_Access :=

          new (Self.Subpool) Program.Nodes.Membership_Tests.Membership_Test'
            (Program.Nodes.Membership_Tests.Create
               (Expression => Expression, Not_Token => Not_Token,
                In_Token => In_Token, Choices => Choices));
   begin
      return Program.Elements.Membership_Tests.Membership_Test_Access (Result);
   end Create_Membership_Test;

   not overriding function Create_Null_Literal
    (Self               : Element_Factory;
     Null_Literal_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Null_Literals.Null_Literal_Access is
      Result : constant Null_Literal_Access :=

          new (Self.Subpool) Program.Nodes.Null_Literals.Null_Literal'
            (Program.Nodes.Null_Literals.Create
               (Null_Literal_Token => Null_Literal_Token));
   begin
      return Program.Elements.Null_Literals.Null_Literal_Access (Result);
   end Create_Null_Literal;

   not overriding function Create_Parenthesized_Expression
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression          : not null Program.Elements.Expressions
         .Expression_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Parenthesized_Expressions
          .Parenthesized_Expression_Access is
      Result : constant Parenthesized_Expression_Access :=

          new (Self.Subpool) Program.Nodes.Parenthesized_Expressions
            .Parenthesized_Expression'
            (Program.Nodes.Parenthesized_Expressions.Create
               (Left_Bracket_Token => Left_Bracket_Token,
                Expression => Expression,
                Right_Bracket_Token => Right_Bracket_Token));
   begin
      return Program.Elements.Parenthesized_Expressions
        .Parenthesized_Expression_Access
        (Result);
   end Create_Parenthesized_Expression;

   not overriding function Create_Raise_Expression
    (Self : Element_Factory;
     Raise_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Exception_Name     : not null Program.Elements.Expressions
         .Expression_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Associated_Message : Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Raise_Expressions
          .Raise_Expression_Access is
      Result : constant Raise_Expression_Access :=

          new (Self.Subpool) Program.Nodes.Raise_Expressions.Raise_Expression'
            (Program.Nodes.Raise_Expressions.Create
               (Raise_Token => Raise_Token, Exception_Name => Exception_Name,
                With_Token => With_Token,
                Associated_Message => Associated_Message));
   begin
      return Program.Elements.Raise_Expressions.Raise_Expression_Access
        (Result);
   end Create_Raise_Expression;

   not overriding function Create_Type_Conversion
    (Self : Element_Factory;
     Subtype_Mark        : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Operand             : not null Program.Elements.Expressions
         .Expression_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Type_Conversions
          .Type_Conversion_Access is
      Result : constant Type_Conversion_Access :=

          new (Self.Subpool) Program.Nodes.Type_Conversions.Type_Conversion'
            (Program.Nodes.Type_Conversions.Create
               (Subtype_Mark => Subtype_Mark,
                Left_Bracket_Token => Left_Bracket_Token, Operand => Operand,
                Right_Bracket_Token => Right_Bracket_Token));
   begin
      return Program.Elements.Type_Conversions.Type_Conversion_Access (Result);
   end Create_Type_Conversion;

   not overriding function Create_Qualified_Expression
    (Self : Element_Factory;
     Subtype_Mark        : not null Program.Elements.Expressions
         .Expression_Access;
     Apostrophe_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Operand             : not null Program.Elements.Expressions
         .Expression_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Qualified_Expressions
          .Qualified_Expression_Access is
      Result : constant Qualified_Expression_Access :=

          new (Self.Subpool) Program.Nodes.Qualified_Expressions
            .Qualified_Expression'
            (Program.Nodes.Qualified_Expressions.Create
               (Subtype_Mark => Subtype_Mark,
                Apostrophe_Token => Apostrophe_Token,
                Left_Bracket_Token => Left_Bracket_Token, Operand => Operand,
                Right_Bracket_Token => Right_Bracket_Token));
   begin
      return Program.Elements.Qualified_Expressions.Qualified_Expression_Access
        (Result);
   end Create_Qualified_Expression;

   not overriding function Create_Allocator
    (Self : Element_Factory;
     New_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Subpool_Name         : Program.Elements.Expressions.Expression_Access;
     Right_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Subtype_Indication   : Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Qualified_Expression : Program.Elements.Qualified_Expressions
         .Qualified_Expression_Access)
      return not null Program.Elements.Allocators.Allocator_Access is
      Result : constant Allocator_Access :=

          new (Self.Subpool) Program.Nodes.Allocators.Allocator'
            (Program.Nodes.Allocators.Create
               (New_Token => New_Token,
                Left_Bracket_Token => Left_Bracket_Token,
                Subpool_Name => Subpool_Name,
                Right_Bracket_Token => Right_Bracket_Token,
                Subtype_Indication => Subtype_Indication,
                Qualified_Expression => Qualified_Expression));
   begin
      return Program.Elements.Allocators.Allocator_Access (Result);
   end Create_Allocator;

   not overriding function Create_Case_Expression
    (Self : Element_Factory;
     Case_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Selecting_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Paths                : not null Program.Elements.Case_Expression_Paths
         .Case_Expression_Path_Vector_Access)
      return not null Program.Elements.Case_Expressions
          .Case_Expression_Access is
      Result : constant Case_Expression_Access :=

          new (Self.Subpool) Program.Nodes.Case_Expressions.Case_Expression'
            (Program.Nodes.Case_Expressions.Create
               (Case_Token => Case_Token,
                Selecting_Expression => Selecting_Expression,
                Is_Token => Is_Token, Paths => Paths));
   begin
      return Program.Elements.Case_Expressions.Case_Expression_Access (Result);
   end Create_Case_Expression;

   not overriding function Create_If_Expression
    (Self : Element_Factory;
     If_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Condition       : not null Program.Elements.Expressions.Expression_Access;
     Then_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Then_Expression : not null Program.Elements.Expressions.Expression_Access;
     Elsif_Paths     : not null Program.Elements.Elsif_Paths
         .Elsif_Path_Vector_Access;
     Else_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Else_Expression : Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.If_Expressions.If_Expression_Access is
      Result : constant If_Expression_Access :=

          new (Self.Subpool) Program.Nodes.If_Expressions.If_Expression'
            (Program.Nodes.If_Expressions.Create
               (If_Token => If_Token, Condition => Condition,
                Then_Token => Then_Token, Then_Expression => Then_Expression,
                Elsif_Paths => Elsif_Paths, Else_Token => Else_Token,
                Else_Expression => Else_Expression));
   begin
      return Program.Elements.If_Expressions.If_Expression_Access (Result);
   end Create_If_Expression;

   not overriding function Create_Quantified_Expression
    (Self : Element_Factory;
     For_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     All_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Some_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Parameter            : Program.Elements.Loop_Parameter_Specifications
         .Loop_Parameter_Specification_Access;
     Generalized_Iterator : Program.Elements
         .Generalized_Iterator_Specifications
         .Generalized_Iterator_Specification_Access;
     Element_Iterator     : Program.Elements.Element_Iterator_Specifications
         .Element_Iterator_Specification_Access;
     Arrow_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Predicate            : not null Program.Elements.Expressions
         .Expression_Access)
      return not null Program.Elements.Quantified_Expressions
          .Quantified_Expression_Access is
      Result : constant Quantified_Expression_Access :=

          new (Self.Subpool) Program.Nodes.Quantified_Expressions
            .Quantified_Expression'
            (Program.Nodes.Quantified_Expressions.Create
               (For_Token => For_Token, All_Token => All_Token,
                Some_Token => Some_Token, Parameter => Parameter,
                Generalized_Iterator => Generalized_Iterator,
                Element_Iterator => Element_Iterator,
                Arrow_Token => Arrow_Token, Predicate => Predicate));
   begin
      return Program.Elements.Quantified_Expressions
        .Quantified_Expression_Access
        (Result);
   end Create_Quantified_Expression;

   not overriding function Create_Discriminant_Association
    (Self : Element_Factory;
     Selector_Names : not null Program.Elements.Identifiers
         .Identifier_Vector_Access;
     Arrow_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Expression     : not null Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Discriminant_Associations
          .Discriminant_Association_Access is
      Result : constant Discriminant_Association_Access :=

          new (Self.Subpool) Program.Nodes.Discriminant_Associations
            .Discriminant_Association'
            (Program.Nodes.Discriminant_Associations.Create
               (Selector_Names => Selector_Names, Arrow_Token => Arrow_Token,
                Expression => Expression));
   begin
      return Program.Elements.Discriminant_Associations
        .Discriminant_Association_Access
        (Result);
   end Create_Discriminant_Association;

   not overriding function Create_Record_Component_Association
    (Self : Element_Factory;
     Choices     : not null Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Expression  : Program.Elements.Expressions.Expression_Access;
     Box_Token   : Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Record_Component_Associations
          .Record_Component_Association_Access is
      Result : constant Record_Component_Association_Access :=

          new (Self.Subpool) Program.Nodes.Record_Component_Associations
            .Record_Component_Association'
            (Program.Nodes.Record_Component_Associations.Create
               (Choices => Choices, Arrow_Token => Arrow_Token,
                Expression => Expression, Box_Token => Box_Token));
   begin
      return Program.Elements.Record_Component_Associations
        .Record_Component_Association_Access
        (Result);
   end Create_Record_Component_Association;

   not overriding function Create_Array_Component_Association
    (Self : Element_Factory;
     Choices     : not null Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Expression  : Program.Elements.Expressions.Expression_Access;
     Box_Token   : Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Array_Component_Associations
          .Array_Component_Association_Access is
      Result : constant Array_Component_Association_Access :=

          new (Self.Subpool) Program.Nodes.Array_Component_Associations
            .Array_Component_Association'
            (Program.Nodes.Array_Component_Associations.Create
               (Choices => Choices, Arrow_Token => Arrow_Token,
                Expression => Expression, Box_Token => Box_Token));
   begin
      return Program.Elements.Array_Component_Associations
        .Array_Component_Association_Access
        (Result);
   end Create_Array_Component_Association;

   not overriding function Create_Parameter_Association
    (Self : Element_Factory;
     Formal_Parameter : Program.Elements.Expressions.Expression_Access;
     Arrow_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Actual_Parameter : not null Program.Elements.Expressions
         .Expression_Access)
      return not null Program.Elements.Parameter_Associations
          .Parameter_Association_Access is
      Result : constant Parameter_Association_Access :=

          new (Self.Subpool) Program.Nodes.Parameter_Associations
            .Parameter_Association'
            (Program.Nodes.Parameter_Associations.Create
               (Formal_Parameter => Formal_Parameter,
                Arrow_Token => Arrow_Token,
                Actual_Parameter => Actual_Parameter));
   begin
      return Program.Elements.Parameter_Associations
        .Parameter_Association_Access
        (Result);
   end Create_Parameter_Association;

   not overriding function Create_Formal_Package_Association
    (Self : Element_Factory;
     Formal_Parameter : Program.Elements.Expressions.Expression_Access;
     Arrow_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Actual_Parameter : Program.Elements.Expressions.Expression_Access;
     Box_Token        : Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Formal_Package_Associations
          .Formal_Package_Association_Access is
      Result : constant Formal_Package_Association_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Package_Associations
            .Formal_Package_Association'
            (Program.Nodes.Formal_Package_Associations.Create
               (Formal_Parameter => Formal_Parameter,
                Arrow_Token => Arrow_Token,
                Actual_Parameter => Actual_Parameter, Box_Token => Box_Token));
   begin
      return Program.Elements.Formal_Package_Associations
        .Formal_Package_Association_Access
        (Result);
   end Create_Formal_Package_Association;

   not overriding function Create_Null_Statement
    (Self : Element_Factory;
     Null_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Null_Statements.Null_Statement_Access is
      Result : constant Null_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Null_Statements.Null_Statement'
            (Program.Nodes.Null_Statements.Create
               (Null_Token => Null_Token, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Null_Statements.Null_Statement_Access (Result);
   end Create_Null_Statement;

   not overriding function Create_Assignment_Statement
    (Self : Element_Factory;
     Variable_Name    : not null Program.Elements.Expressions
         .Expression_Access;
     Assignment_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression       : not null Program.Elements.Expressions
         .Expression_Access;
     Semicolon_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Assignment_Statements
          .Assignment_Statement_Access is
      Result : constant Assignment_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Assignment_Statements
            .Assignment_Statement'
            (Program.Nodes.Assignment_Statements.Create
               (Variable_Name => Variable_Name,
                Assignment_Token => Assignment_Token, Expression => Expression,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Assignment_Statements.Assignment_Statement_Access
        (Result);
   end Create_Assignment_Statement;

   not overriding function Create_If_Statement
    (Self : Element_Factory;
     If_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Condition       : not null Program.Elements.Expressions.Expression_Access;
     Then_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Then_Statements : not null Program.Element_Vectors.Element_Vector_Access;
     Elsif_Paths     : not null Program.Elements.Elsif_Paths
         .Elsif_Path_Vector_Access;
     Else_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Else_Statements : not null Program.Element_Vectors.Element_Vector_Access;
     End_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     If_Token_2      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.If_Statements.If_Statement_Access is
      Result : constant If_Statement_Access :=

          new (Self.Subpool) Program.Nodes.If_Statements.If_Statement'
            (Program.Nodes.If_Statements.Create
               (If_Token => If_Token, Condition => Condition,
                Then_Token => Then_Token, Then_Statements => Then_Statements,
                Elsif_Paths => Elsif_Paths, Else_Token => Else_Token,
                Else_Statements => Else_Statements, End_Token => End_Token,
                If_Token_2 => If_Token_2, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.If_Statements.If_Statement_Access (Result);
   end Create_If_Statement;

   not overriding function Create_Case_Statement
    (Self : Element_Factory;
     Case_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Selecting_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Paths                : not null Program.Elements.Case_Paths
         .Case_Path_Vector_Access;
     End_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Case_Token_2         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Case_Statements.Case_Statement_Access is
      Result : constant Case_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Case_Statements.Case_Statement'
            (Program.Nodes.Case_Statements.Create
               (Case_Token => Case_Token,
                Selecting_Expression => Selecting_Expression,
                Is_Token => Is_Token, Paths => Paths, End_Token => End_Token,
                Case_Token_2 => Case_Token_2,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Case_Statements.Case_Statement_Access (Result);
   end Create_Case_Statement;

   not overriding function Create_Loop_Statement
    (Self : Element_Factory;
     Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token              : Program.Lexical_Elements
         .Lexical_Element_Access;
     Loop_Token               : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Token                : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Loop_Token_2             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Loop_Statements.Loop_Statement_Access is
      Result : constant Loop_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Loop_Statements.Loop_Statement'
            (Program.Nodes.Loop_Statements.Create
               (Statement_Identifier => Statement_Identifier,
                Colon_Token => Colon_Token, Loop_Token => Loop_Token,
                Statements => Statements, End_Token => End_Token,
                Loop_Token_2 => Loop_Token_2,
                End_Statement_Identifier => End_Statement_Identifier,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Loop_Statements.Loop_Statement_Access (Result);
   end Create_Loop_Statement;

   not overriding function Create_While_Loop_Statement
    (Self : Element_Factory;
     Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token              : Program.Lexical_Elements
         .Lexical_Element_Access;
     While_Token              : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Condition                : not null Program.Elements.Expressions
         .Expression_Access;
     Loop_Token               : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Token                : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Loop_Token_2             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.While_Loop_Statements
          .While_Loop_Statement_Access is
      Result : constant While_Loop_Statement_Access :=

          new (Self.Subpool) Program.Nodes.While_Loop_Statements
            .While_Loop_Statement'
            (Program.Nodes.While_Loop_Statements.Create
               (Statement_Identifier => Statement_Identifier,
                Colon_Token => Colon_Token, While_Token => While_Token,
                Condition => Condition, Loop_Token => Loop_Token,
                Statements => Statements, End_Token => End_Token,
                Loop_Token_2 => Loop_Token_2,
                End_Statement_Identifier => End_Statement_Identifier,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.While_Loop_Statements.While_Loop_Statement_Access
        (Result);
   end Create_While_Loop_Statement;

   not overriding function Create_For_Loop_Statement
    (Self : Element_Factory;
     Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token              : Program.Lexical_Elements
         .Lexical_Element_Access;
     For_Token                : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Loop_Parameter           : Program.Elements.Loop_Parameter_Specifications
         .Loop_Parameter_Specification_Access;
     Generalized_Iterator     : Program.Elements
         .Generalized_Iterator_Specifications
         .Generalized_Iterator_Specification_Access;
     Element_Iterator         : Program.Elements
         .Element_Iterator_Specifications
         .Element_Iterator_Specification_Access;
     Loop_Token               : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Token                : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Loop_Token_2             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.For_Loop_Statements
          .For_Loop_Statement_Access is
      Result : constant For_Loop_Statement_Access :=

          new (Self.Subpool) Program.Nodes.For_Loop_Statements
            .For_Loop_Statement'
            (Program.Nodes.For_Loop_Statements.Create
               (Statement_Identifier => Statement_Identifier,
                Colon_Token => Colon_Token, For_Token => For_Token,
                Loop_Parameter => Loop_Parameter,
                Generalized_Iterator => Generalized_Iterator,
                Element_Iterator => Element_Iterator, Loop_Token => Loop_Token,
                Statements => Statements, End_Token => End_Token,
                Loop_Token_2 => Loop_Token_2,
                End_Statement_Identifier => End_Statement_Identifier,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.For_Loop_Statements.For_Loop_Statement_Access
        (Result);
   end Create_For_Loop_Statement;

   not overriding function Create_Block_Statement
    (Self : Element_Factory;
     Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token              : Program.Lexical_Elements
         .Lexical_Element_Access;
     Declare_Token            : Program.Lexical_Elements
         .Lexical_Element_Access;
     Declarations             : not null Program.Element_Vectors
         .Element_Vector_Access;
     Begin_Token              : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token          : Program.Lexical_Elements
         .Lexical_Element_Access;
     Exception_Handlers       : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token                : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Block_Statements
          .Block_Statement_Access is
      Result : constant Block_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Block_Statements.Block_Statement'
            (Program.Nodes.Block_Statements.Create
               (Statement_Identifier => Statement_Identifier,
                Colon_Token => Colon_Token, Declare_Token => Declare_Token,
                Declarations => Declarations, Begin_Token => Begin_Token,
                Statements => Statements, Exception_Token => Exception_Token,
                Exception_Handlers => Exception_Handlers,
                End_Token => End_Token,
                End_Statement_Identifier => End_Statement_Identifier,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Block_Statements.Block_Statement_Access (Result);
   end Create_Block_Statement;

   not overriding function Create_Exit_Statement
    (Self : Element_Factory;
     Exit_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Exit_Loop_Name  : Program.Elements.Expressions.Expression_Access;
     When_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Condition       : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Exit_Statements.Exit_Statement_Access is
      Result : constant Exit_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Exit_Statements.Exit_Statement'
            (Program.Nodes.Exit_Statements.Create
               (Exit_Token => Exit_Token, Exit_Loop_Name => Exit_Loop_Name,
                When_Token => When_Token, Condition => Condition,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Exit_Statements.Exit_Statement_Access (Result);
   end Create_Exit_Statement;

   not overriding function Create_Goto_Statement
    (Self : Element_Factory;
     Goto_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Goto_Label      : not null Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Goto_Statements.Goto_Statement_Access is
      Result : constant Goto_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Goto_Statements.Goto_Statement'
            (Program.Nodes.Goto_Statements.Create
               (Goto_Token => Goto_Token, Goto_Label => Goto_Label,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Goto_Statements.Goto_Statement_Access (Result);
   end Create_Goto_Statement;

   not overriding function Create_Call_Statement
    (Self : Element_Factory;
     Called_Name         : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Call_Statements.Call_Statement_Access is
      Result : constant Call_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Call_Statements.Call_Statement'
            (Program.Nodes.Call_Statements.Create
               (Called_Name => Called_Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Call_Statements.Call_Statement_Access (Result);
   end Create_Call_Statement;

   not overriding function Create_Simple_Return_Statement
    (Self : Element_Factory;
     Return_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression      : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Simple_Return_Statements
          .Simple_Return_Statement_Access is
      Result : constant Simple_Return_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Simple_Return_Statements
            .Simple_Return_Statement'
            (Program.Nodes.Simple_Return_Statements.Create
               (Return_Token => Return_Token, Expression => Expression,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Simple_Return_Statements
        .Simple_Return_Statement_Access
        (Result);
   end Create_Simple_Return_Statement;

   not overriding function Create_Extended_Return_Statement
    (Self : Element_Factory;
     Return_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Return_Object      : not null Program.Elements
         .Return_Object_Specifications.Return_Object_Specification_Access;
     Do_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Statements         : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Exception_Handlers : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token_2     : Program.Lexical_Elements.Lexical_Element_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Extended_Return_Statements
          .Extended_Return_Statement_Access is
      Result : constant Extended_Return_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Extended_Return_Statements
            .Extended_Return_Statement'
            (Program.Nodes.Extended_Return_Statements.Create
               (Return_Token => Return_Token, Return_Object => Return_Object,
                Do_Token => Do_Token, Statements => Statements,
                Exception_Token => Exception_Token,
                Exception_Handlers => Exception_Handlers,
                End_Token => End_Token, Return_Token_2 => Return_Token_2,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Extended_Return_Statements
        .Extended_Return_Statement_Access
        (Result);
   end Create_Extended_Return_Statement;

   not overriding function Create_Accept_Statement
    (Self : Element_Factory;
     Accept_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Entry_Name               : not null Program.Elements.Identifiers
         .Identifier_Access;
     Left_Bracket_Token       : Program.Lexical_Elements
         .Lexical_Element_Access;
     Entry_Index              : Program.Elements.Expressions.Expression_Access;
     Right_Bracket_Token      : Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token_2     : Program.Lexical_Elements
         .Lexical_Element_Access;
     Parameters               : not null Program.Elements
         .Parameter_Specifications.Parameter_Specification_Vector_Access;
     Right_Bracket_Token_2    : Program.Lexical_Elements
         .Lexical_Element_Access;
     Do_Token                 : Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token          : Program.Lexical_Elements
         .Lexical_Element_Access;
     Exception_Handlers       : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token                : Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Accept_Statements
          .Accept_Statement_Access is
      Result : constant Accept_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Accept_Statements.Accept_Statement'
            (Program.Nodes.Accept_Statements.Create
               (Accept_Token => Accept_Token, Entry_Name => Entry_Name,
                Left_Bracket_Token => Left_Bracket_Token,
                Entry_Index => Entry_Index,
                Right_Bracket_Token => Right_Bracket_Token,
                Left_Bracket_Token_2 => Left_Bracket_Token_2,
                Parameters => Parameters,
                Right_Bracket_Token_2 => Right_Bracket_Token_2,
                Do_Token => Do_Token, Statements => Statements,
                Exception_Token => Exception_Token,
                Exception_Handlers => Exception_Handlers,
                End_Token => End_Token,
                End_Statement_Identifier => End_Statement_Identifier,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Accept_Statements.Accept_Statement_Access
        (Result);
   end Create_Accept_Statement;

   not overriding function Create_Requeue_Statement
    (Self : Element_Factory;
     Requeue_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Entry_Name      : not null Program.Elements.Expressions.Expression_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Abort_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Requeue_Statements
          .Requeue_Statement_Access is
      Result : constant Requeue_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Requeue_Statements
            .Requeue_Statement'
            (Program.Nodes.Requeue_Statements.Create
               (Requeue_Token => Requeue_Token, Entry_Name => Entry_Name,
                With_Token => With_Token, Abort_Token => Abort_Token,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Requeue_Statements.Requeue_Statement_Access
        (Result);
   end Create_Requeue_Statement;

   not overriding function Create_Delay_Statement
    (Self : Element_Factory;
     Delay_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Until_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression      : not null Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Delay_Statements
          .Delay_Statement_Access is
      Result : constant Delay_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Delay_Statements.Delay_Statement'
            (Program.Nodes.Delay_Statements.Create
               (Delay_Token => Delay_Token, Until_Token => Until_Token,
                Expression => Expression, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Delay_Statements.Delay_Statement_Access (Result);
   end Create_Delay_Statement;

   not overriding function Create_Terminate_Alternative_Statement
    (Self : Element_Factory;
     Terminate_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Terminate_Alternative_Statements
          .Terminate_Alternative_Statement_Access is
      Result : constant Terminate_Alternative_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Terminate_Alternative_Statements
            .Terminate_Alternative_Statement'
            (Program.Nodes.Terminate_Alternative_Statements.Create
               (Terminate_Token => Terminate_Token,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Terminate_Alternative_Statements
        .Terminate_Alternative_Statement_Access
        (Result);
   end Create_Terminate_Alternative_Statement;

   not overriding function Create_Select_Statement
    (Self : Element_Factory;
     Select_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Paths                 : not null Program.Elements.Select_Paths
         .Select_Path_Vector_Access;
     Then_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Abort_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Then_Abort_Statements : not null Program.Element_Vectors
         .Element_Vector_Access;
     Else_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Else_Statements       : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Select_Token_2        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Select_Statements
          .Select_Statement_Access is
      Result : constant Select_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Select_Statements.Select_Statement'
            (Program.Nodes.Select_Statements.Create
               (Select_Token => Select_Token, Paths => Paths,
                Then_Token => Then_Token, Abort_Token => Abort_Token,
                Then_Abort_Statements => Then_Abort_Statements,
                Else_Token => Else_Token, Else_Statements => Else_Statements,
                End_Token => End_Token, Select_Token_2 => Select_Token_2,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Select_Statements.Select_Statement_Access
        (Result);
   end Create_Select_Statement;

   not overriding function Create_Abort_Statement
    (Self : Element_Factory;
     Abort_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Aborted_Tasks   : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Abort_Statements
          .Abort_Statement_Access is
      Result : constant Abort_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Abort_Statements.Abort_Statement'
            (Program.Nodes.Abort_Statements.Create
               (Abort_Token => Abort_Token, Aborted_Tasks => Aborted_Tasks,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Abort_Statements.Abort_Statement_Access (Result);
   end Create_Abort_Statement;

   not overriding function Create_Raise_Statement
    (Self : Element_Factory;
     Raise_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Raised_Exception   : Program.Elements.Expressions.Expression_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Associated_Message : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Raise_Statements
          .Raise_Statement_Access is
      Result : constant Raise_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Raise_Statements.Raise_Statement'
            (Program.Nodes.Raise_Statements.Create
               (Raise_Token => Raise_Token,
                Raised_Exception => Raised_Exception, With_Token => With_Token,
                Associated_Message => Associated_Message,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Raise_Statements.Raise_Statement_Access (Result);
   end Create_Raise_Statement;

   not overriding function Create_Code_Statement
    (Self : Element_Factory;
     Expression      : not null Program.Elements.Qualified_Expressions
         .Qualified_Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Code_Statements.Code_Statement_Access is
      Result : constant Code_Statement_Access :=

          new (Self.Subpool) Program.Nodes.Code_Statements.Code_Statement'
            (Program.Nodes.Code_Statements.Create
               (Expression => Expression, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Code_Statements.Code_Statement_Access (Result);
   end Create_Code_Statement;

   not overriding function Create_Elsif_Path
    (Self : Element_Factory;
     Elsif_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Condition   : not null Program.Elements.Expressions.Expression_Access;
     Then_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Statements  : not null Program.Element_Vectors.Element_Vector_Access)
      return not null Program.Elements.Elsif_Paths.Elsif_Path_Access is
      Result : constant Elsif_Path_Access :=

          new (Self.Subpool) Program.Nodes.Elsif_Paths.Elsif_Path'
            (Program.Nodes.Elsif_Paths.Create
               (Elsif_Token => Elsif_Token, Condition => Condition,
                Then_Token => Then_Token, Statements => Statements));
   begin
      return Program.Elements.Elsif_Paths.Elsif_Path_Access (Result);
   end Create_Elsif_Path;

   not overriding function Create_Case_Path
    (Self : Element_Factory;
     When_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Choices     : not null Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Statements  : not null Program.Element_Vectors.Element_Vector_Access)
      return not null Program.Elements.Case_Paths.Case_Path_Access is
      Result : constant Case_Path_Access :=

          new (Self.Subpool) Program.Nodes.Case_Paths.Case_Path'
            (Program.Nodes.Case_Paths.Create
               (When_Token => When_Token, Choices => Choices,
                Arrow_Token => Arrow_Token, Statements => Statements));
   begin
      return Program.Elements.Case_Paths.Case_Path_Access (Result);
   end Create_Case_Path;

   not overriding function Create_Select_Path
    (Self : Element_Factory;
     When_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Guard       : Program.Elements.Expressions.Expression_Access;
     Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Statements  : not null Program.Element_Vectors.Element_Vector_Access)
      return not null Program.Elements.Select_Paths.Select_Path_Access is
      Result : constant Select_Path_Access :=

          new (Self.Subpool) Program.Nodes.Select_Paths.Select_Path'
            (Program.Nodes.Select_Paths.Create
               (When_Token => When_Token, Guard => Guard,
                Arrow_Token => Arrow_Token, Statements => Statements));
   begin
      return Program.Elements.Select_Paths.Select_Path_Access (Result);
   end Create_Select_Path;

   not overriding function Create_Case_Expression_Path
    (Self : Element_Factory;
     When_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Choices     : not null Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Expression  : not null Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Case_Expression_Paths
          .Case_Expression_Path_Access is
      Result : constant Case_Expression_Path_Access :=

          new (Self.Subpool) Program.Nodes.Case_Expression_Paths
            .Case_Expression_Path'
            (Program.Nodes.Case_Expression_Paths.Create
               (When_Token => When_Token, Choices => Choices,
                Arrow_Token => Arrow_Token, Expression => Expression));
   begin
      return Program.Elements.Case_Expression_Paths.Case_Expression_Path_Access
        (Result);
   end Create_Case_Expression_Path;

   not overriding function Create_Elsif_Expression_Path
    (Self : Element_Factory;
     Elsif_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Condition   : not null Program.Elements.Expressions.Expression_Access;
     Then_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Expression  : not null Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Elsif_Expression_Paths
          .Elsif_Expression_Path_Access is
      Result : constant Elsif_Expression_Path_Access :=

          new (Self.Subpool) Program.Nodes.Elsif_Expression_Paths
            .Elsif_Expression_Path'
            (Program.Nodes.Elsif_Expression_Paths.Create
               (Elsif_Token => Elsif_Token, Condition => Condition,
                Then_Token => Then_Token, Expression => Expression));
   begin
      return Program.Elements.Elsif_Expression_Paths
        .Elsif_Expression_Path_Access
        (Result);
   end Create_Elsif_Expression_Path;

   not overriding function Create_Use_Clause
    (Self : Element_Factory;
     Use_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     All_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Type_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Clause_Names    : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Use_Clauses.Use_Clause_Access is
      Result : constant Use_Clause_Access :=

          new (Self.Subpool) Program.Nodes.Use_Clauses.Use_Clause'
            (Program.Nodes.Use_Clauses.Create
               (Use_Token => Use_Token, All_Token => All_Token,
                Type_Token => Type_Token, Clause_Names => Clause_Names,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Use_Clauses.Use_Clause_Access (Result);
   end Create_Use_Clause;

   not overriding function Create_With_Clause
    (Self : Element_Factory;
     Limited_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Clause_Names    : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.With_Clauses.With_Clause_Access is
      Result : constant With_Clause_Access :=

          new (Self.Subpool) Program.Nodes.With_Clauses.With_Clause'
            (Program.Nodes.With_Clauses.Create
               (Limited_Token => Limited_Token, Private_Token => Private_Token,
                With_Token => With_Token, Clause_Names => Clause_Names,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.With_Clauses.With_Clause_Access (Result);
   end Create_With_Clause;

   not overriding function Create_Component_Clause
    (Self : Element_Factory;
     Clause_Name     : not null Program.Elements.Identifiers.Identifier_Access;
     At_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Position        : not null Program.Elements.Expressions.Expression_Access;
     Range_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Clause_Range    : not null Program.Elements.Simple_Expression_Ranges
         .Simple_Expression_Range_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Component_Clauses
          .Component_Clause_Access is
      Result : constant Component_Clause_Access :=

          new (Self.Subpool) Program.Nodes.Component_Clauses.Component_Clause'
            (Program.Nodes.Component_Clauses.Create
               (Clause_Name => Clause_Name, At_Token => At_Token,
                Position => Position, Range_Token => Range_Token,
                Clause_Range => Clause_Range,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Component_Clauses.Component_Clause_Access
        (Result);
   end Create_Component_Clause;

   not overriding function Create_Derived_Type
    (Self : Element_Factory;
     Abstract_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Limited_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     New_Token      : not null Program.Lexical_Elements.Lexical_Element_Access;
     Parent         : not null Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Derived_Types.Derived_Type_Access is
      Result : constant Derived_Type_Access :=

          new (Self.Subpool) Program.Nodes.Derived_Types.Derived_Type'
            (Program.Nodes.Derived_Types.Create
               (Abstract_Token => Abstract_Token,
                Limited_Token => Limited_Token, New_Token => New_Token,
                Parent => Parent));
   begin
      return Program.Elements.Derived_Types.Derived_Type_Access (Result);
   end Create_Derived_Type;

   not overriding function Create_Derived_Record_Extension
    (Self : Element_Factory;
     Abstract_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Limited_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     New_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Parent            : not null Program.Elements.Expressions
         .Expression_Access;
     And_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors       : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     With_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Record_Definition : not null Program.Elements.Definitions
         .Definition_Access)
      return not null Program.Elements.Derived_Record_Extensions
          .Derived_Record_Extension_Access is
      Result : constant Derived_Record_Extension_Access :=

          new (Self.Subpool) Program.Nodes.Derived_Record_Extensions
            .Derived_Record_Extension'
            (Program.Nodes.Derived_Record_Extensions.Create
               (Abstract_Token => Abstract_Token,
                Limited_Token => Limited_Token, New_Token => New_Token,
                Parent => Parent, And_Token => And_Token,
                Progenitors => Progenitors, With_Token => With_Token,
                Record_Definition => Record_Definition));
   begin
      return Program.Elements.Derived_Record_Extensions
        .Derived_Record_Extension_Access
        (Result);
   end Create_Derived_Record_Extension;

   not overriding function Create_Enumeration_Type
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Literals            : not null Program.Elements
         .Enumeration_Literal_Specifications
         .Enumeration_Literal_Specification_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Enumeration_Types
          .Enumeration_Type_Access is
      Result : constant Enumeration_Type_Access :=

          new (Self.Subpool) Program.Nodes.Enumeration_Types.Enumeration_Type'
            (Program.Nodes.Enumeration_Types.Create
               (Left_Bracket_Token => Left_Bracket_Token, Literals => Literals,
                Right_Bracket_Token => Right_Bracket_Token));
   begin
      return Program.Elements.Enumeration_Types.Enumeration_Type_Access
        (Result);
   end Create_Enumeration_Type;

   not overriding function Create_Signed_Integer_Type
    (Self : Element_Factory;
     Range_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Lower_Bound      : not null Program.Elements.Expressions
         .Expression_Access;
     Double_Dot_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Upper_Bound      : not null Program.Elements.Expressions
         .Expression_Access)
      return not null Program.Elements.Signed_Integer_Types
          .Signed_Integer_Type_Access is
      Result : constant Signed_Integer_Type_Access :=

          new (Self.Subpool) Program.Nodes.Signed_Integer_Types
            .Signed_Integer_Type'
            (Program.Nodes.Signed_Integer_Types.Create
               (Range_Token => Range_Token, Lower_Bound => Lower_Bound,
                Double_Dot_Token => Double_Dot_Token,
                Upper_Bound => Upper_Bound));
   begin
      return Program.Elements.Signed_Integer_Types.Signed_Integer_Type_Access
        (Result);
   end Create_Signed_Integer_Type;

   not overriding function Create_Modular_Type
    (Self : Element_Factory;
     Mod_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Modulus   : not null Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Modular_Types.Modular_Type_Access is
      Result : constant Modular_Type_Access :=

          new (Self.Subpool) Program.Nodes.Modular_Types.Modular_Type'
            (Program.Nodes.Modular_Types.Create
               (Mod_Token => Mod_Token, Modulus => Modulus));
   begin
      return Program.Elements.Modular_Types.Modular_Type_Access (Result);
   end Create_Modular_Type;

   not overriding function Create_Root_Type
    (Self : Element_Factory)
      return not null Program.Elements.Root_Types.Root_Type_Access is
      Result : constant Root_Type_Access :=
         new (Self.Subpool) Program.Nodes.Root_Types.Root_Type;
   begin
      return Program.Elements.Root_Types.Root_Type_Access (Result);
   end Create_Root_Type;

   not overriding function Create_Floating_Point_Type
    (Self : Element_Factory;
     Digits_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Digits_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range        : Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access)
      return not null Program.Elements.Floating_Point_Types
          .Floating_Point_Type_Access is
      Result : constant Floating_Point_Type_Access :=

          new (Self.Subpool) Program.Nodes.Floating_Point_Types
            .Floating_Point_Type'
            (Program.Nodes.Floating_Point_Types.Create
               (Digits_Token => Digits_Token,
                Digits_Expression => Digits_Expression,
                Real_Range => Real_Range));
   begin
      return Program.Elements.Floating_Point_Types.Floating_Point_Type_Access
        (Result);
   end Create_Floating_Point_Type;

   not overriding function Create_Ordinary_Fixed_Point_Type
    (Self : Element_Factory;
     Delta_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Delta_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range       : not null Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access)
      return not null Program.Elements.Ordinary_Fixed_Point_Types
          .Ordinary_Fixed_Point_Type_Access is
      Result : constant Ordinary_Fixed_Point_Type_Access :=

          new (Self.Subpool) Program.Nodes.Ordinary_Fixed_Point_Types
            .Ordinary_Fixed_Point_Type'
            (Program.Nodes.Ordinary_Fixed_Point_Types.Create
               (Delta_Token => Delta_Token,
                Delta_Expression => Delta_Expression,
                Real_Range => Real_Range));
   begin
      return Program.Elements.Ordinary_Fixed_Point_Types
        .Ordinary_Fixed_Point_Type_Access
        (Result);
   end Create_Ordinary_Fixed_Point_Type;

   not overriding function Create_Decimal_Fixed_Point_Type
    (Self : Element_Factory;
     Delta_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Delta_Expression  : not null Program.Elements.Expressions
         .Expression_Access;
     Digits_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Digits_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range        : Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access)
      return not null Program.Elements.Decimal_Fixed_Point_Types
          .Decimal_Fixed_Point_Type_Access is
      Result : constant Decimal_Fixed_Point_Type_Access :=

          new (Self.Subpool) Program.Nodes.Decimal_Fixed_Point_Types
            .Decimal_Fixed_Point_Type'
            (Program.Nodes.Decimal_Fixed_Point_Types.Create
               (Delta_Token => Delta_Token,
                Delta_Expression => Delta_Expression,
                Digits_Token => Digits_Token,
                Digits_Expression => Digits_Expression,
                Real_Range => Real_Range));
   begin
      return Program.Elements.Decimal_Fixed_Point_Types
        .Decimal_Fixed_Point_Type_Access
        (Result);
   end Create_Decimal_Fixed_Point_Type;

   not overriding function Create_Unconstrained_Array_Type
    (Self : Element_Factory;
     Array_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Index_Subtypes       : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Right_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Of_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Component_Definition : not null Program.Elements.Component_Definitions
         .Component_Definition_Access)
      return not null Program.Elements.Unconstrained_Array_Types
          .Unconstrained_Array_Type_Access is
      Result : constant Unconstrained_Array_Type_Access :=

          new (Self.Subpool) Program.Nodes.Unconstrained_Array_Types
            .Unconstrained_Array_Type'
            (Program.Nodes.Unconstrained_Array_Types.Create
               (Array_Token => Array_Token,
                Left_Bracket_Token => Left_Bracket_Token,
                Index_Subtypes => Index_Subtypes,
                Right_Bracket_Token => Right_Bracket_Token,
                Of_Token => Of_Token,
                Component_Definition => Component_Definition));
   begin
      return Program.Elements.Unconstrained_Array_Types
        .Unconstrained_Array_Type_Access
        (Result);
   end Create_Unconstrained_Array_Type;

   not overriding function Create_Constrained_Array_Type
    (Self : Element_Factory;
     Array_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Index_Subtypes       : not null Program.Elements
         .Discrete_Subtype_Definitions
         .Discrete_Subtype_Definition_Vector_Access;
     Right_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Of_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Component_Definition : not null Program.Elements.Component_Definitions
         .Component_Definition_Access)
      return not null Program.Elements.Constrained_Array_Types
          .Constrained_Array_Type_Access is
      Result : constant Constrained_Array_Type_Access :=

          new (Self.Subpool) Program.Nodes.Constrained_Array_Types
            .Constrained_Array_Type'
            (Program.Nodes.Constrained_Array_Types.Create
               (Array_Token => Array_Token,
                Left_Bracket_Token => Left_Bracket_Token,
                Index_Subtypes => Index_Subtypes,
                Right_Bracket_Token => Right_Bracket_Token,
                Of_Token => Of_Token,
                Component_Definition => Component_Definition));
   begin
      return Program.Elements.Constrained_Array_Types
        .Constrained_Array_Type_Access
        (Result);
   end Create_Constrained_Array_Type;

   not overriding function Create_Record_Type
    (Self : Element_Factory;
     Abstract_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Tagged_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Limited_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Record_Definition : not null Program.Elements.Definitions
         .Definition_Access)
      return not null Program.Elements.Record_Types.Record_Type_Access is
      Result : constant Record_Type_Access :=

          new (Self.Subpool) Program.Nodes.Record_Types.Record_Type'
            (Program.Nodes.Record_Types.Create
               (Abstract_Token => Abstract_Token, Tagged_Token => Tagged_Token,
                Limited_Token => Limited_Token,
                Record_Definition => Record_Definition));
   begin
      return Program.Elements.Record_Types.Record_Type_Access (Result);
   end Create_Record_Type;

   not overriding function Create_Interface_Type
    (Self : Element_Factory;
     Limited_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Task_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Protected_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Synchronized_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Interface_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     And_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors        : not null Program.Elements.Expressions
         .Expression_Vector_Access)
      return not null Program.Elements.Interface_Types.Interface_Type_Access is
      Result : constant Interface_Type_Access :=

          new (Self.Subpool) Program.Nodes.Interface_Types.Interface_Type'
            (Program.Nodes.Interface_Types.Create
               (Limited_Token => Limited_Token, Task_Token => Task_Token,
                Protected_Token => Protected_Token,
                Synchronized_Token => Synchronized_Token,
                Interface_Token => Interface_Token, And_Token => And_Token,
                Progenitors => Progenitors));
   begin
      return Program.Elements.Interface_Types.Interface_Type_Access (Result);
   end Create_Interface_Type;

   not overriding function Create_Object_Access_Type
    (Self : Element_Factory;
     Not_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Access_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     All_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Constant_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Subtype_Indication : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access)
      return not null Program.Elements.Object_Access_Types
          .Object_Access_Type_Access is
      Result : constant Object_Access_Type_Access :=

          new (Self.Subpool) Program.Nodes.Object_Access_Types
            .Object_Access_Type'
            (Program.Nodes.Object_Access_Types.Create
               (Not_Token => Not_Token, Null_Token => Null_Token,
                Access_Token => Access_Token, All_Token => All_Token,
                Constant_Token => Constant_Token,
                Subtype_Indication => Subtype_Indication));
   begin
      return Program.Elements.Object_Access_Types.Object_Access_Type_Access
        (Result);
   end Create_Object_Access_Type;

   not overriding function Create_Procedure_Access_Type
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Access_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Protected_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Procedure_Access_Types
          .Procedure_Access_Type_Access is
      Result : constant Procedure_Access_Type_Access :=

          new (Self.Subpool) Program.Nodes.Procedure_Access_Types
            .Procedure_Access_Type'
            (Program.Nodes.Procedure_Access_Types.Create
               (Not_Token => Not_Token, Null_Token => Null_Token,
                Access_Token => Access_Token,
                Protected_Token => Protected_Token,
                Procedure_Token => Procedure_Token,
                Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token));
   begin
      return Program.Elements.Procedure_Access_Types
        .Procedure_Access_Type_Access
        (Result);
   end Create_Procedure_Access_Type;

   not overriding function Create_Function_Access_Type
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Access_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Protected_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token_2         : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token_2        : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Subtype      : not null Program.Elements.Element_Access)
      return not null Program.Elements.Function_Access_Types
          .Function_Access_Type_Access is
      Result : constant Function_Access_Type_Access :=

          new (Self.Subpool) Program.Nodes.Function_Access_Types
            .Function_Access_Type'
            (Program.Nodes.Function_Access_Types.Create
               (Not_Token => Not_Token, Null_Token => Null_Token,
                Access_Token => Access_Token,
                Protected_Token => Protected_Token,
                Function_Token => Function_Token,
                Left_Bracket_Token => Left_Bracket_Token,
                Parameters => Parameters,
                Right_Bracket_Token => Right_Bracket_Token,
                Return_Token => Return_Token, Not_Token_2 => Not_Token_2,
                Null_Token_2 => Null_Token_2,
                Result_Subtype => Result_Subtype));
   begin
      return Program.Elements.Function_Access_Types.Function_Access_Type_Access
        (Result);
   end Create_Function_Access_Type;

   not overriding function Create_Formal_Private_Type_Definition
    (Self : Element_Factory;
     Abstract_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Tagged_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Limited_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Token  : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Formal_Private_Type_Definitions
          .Formal_Private_Type_Definition_Access is
      Result : constant Formal_Private_Type_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Private_Type_Definitions
            .Formal_Private_Type_Definition'
            (Program.Nodes.Formal_Private_Type_Definitions.Create
               (Abstract_Token => Abstract_Token, Tagged_Token => Tagged_Token,
                Limited_Token => Limited_Token,
                Private_Token => Private_Token));
   begin
      return Program.Elements.Formal_Private_Type_Definitions
        .Formal_Private_Type_Definition_Access
        (Result);
   end Create_Formal_Private_Type_Definition;

   not overriding function Create_Formal_Derived_Type_Definition
    (Self : Element_Factory;
     Abstract_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Limited_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Synchronized_Token : Program.Lexical_Elements.Lexical_Element_Access;
     New_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Subtype_Mark       : not null Program.Elements.Expressions
         .Expression_Access;
     And_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors        : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Token      : Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Formal_Derived_Type_Definitions
          .Formal_Derived_Type_Definition_Access is
      Result : constant Formal_Derived_Type_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Derived_Type_Definitions
            .Formal_Derived_Type_Definition'
            (Program.Nodes.Formal_Derived_Type_Definitions.Create
               (Abstract_Token => Abstract_Token,
                Limited_Token => Limited_Token,
                Synchronized_Token => Synchronized_Token,
                New_Token => New_Token, Subtype_Mark => Subtype_Mark,
                And_Token => And_Token, Progenitors => Progenitors,
                With_Token => With_Token, Private_Token => Private_Token));
   begin
      return Program.Elements.Formal_Derived_Type_Definitions
        .Formal_Derived_Type_Definition_Access
        (Result);
   end Create_Formal_Derived_Type_Definition;

   not overriding function Create_Formal_Discrete_Type_Definition
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Box_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Formal_Discrete_Type_Definitions
          .Formal_Discrete_Type_Definition_Access is
      Result : constant Formal_Discrete_Type_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Discrete_Type_Definitions
            .Formal_Discrete_Type_Definition'
            (Program.Nodes.Formal_Discrete_Type_Definitions.Create
               (Left_Bracket_Token => Left_Bracket_Token,
                Box_Token => Box_Token,
                Right_Bracket_Token => Right_Bracket_Token));
   begin
      return Program.Elements.Formal_Discrete_Type_Definitions
        .Formal_Discrete_Type_Definition_Access
        (Result);
   end Create_Formal_Discrete_Type_Definition;

   not overriding function Create_Formal_Signed_Integer_Type_Definition
    (Self : Element_Factory;
     Range_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Box_Token   : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Formal_Signed_Integer_Type_Definitions
          .Formal_Signed_Integer_Type_Definition_Access is
      Result : constant Formal_Signed_Integer_Type_Definition_Access :=

          new (Self.Subpool) Program.Nodes
            .Formal_Signed_Integer_Type_Definitions
            .Formal_Signed_Integer_Type_Definition'
            (Program.Nodes.Formal_Signed_Integer_Type_Definitions.Create
               (Range_Token => Range_Token, Box_Token => Box_Token));
   begin
      return Program.Elements.Formal_Signed_Integer_Type_Definitions
        .Formal_Signed_Integer_Type_Definition_Access
        (Result);
   end Create_Formal_Signed_Integer_Type_Definition;

   not overriding function Create_Formal_Modular_Type_Definition
    (Self : Element_Factory;
     Mod_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Box_Token : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Formal_Modular_Type_Definitions
          .Formal_Modular_Type_Definition_Access is
      Result : constant Formal_Modular_Type_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Modular_Type_Definitions
            .Formal_Modular_Type_Definition'
            (Program.Nodes.Formal_Modular_Type_Definitions.Create
               (Mod_Token => Mod_Token, Box_Token => Box_Token));
   begin
      return Program.Elements.Formal_Modular_Type_Definitions
        .Formal_Modular_Type_Definition_Access
        (Result);
   end Create_Formal_Modular_Type_Definition;

   not overriding function Create_Formal_Floating_Point_Definition
    (Self : Element_Factory;
     Digits_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Box_Token    : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Formal_Floating_Point_Definitions
          .Formal_Floating_Point_Definition_Access is
      Result : constant Formal_Floating_Point_Definition_Access :=

          new (Self.Subpool) Program.Nodes.Formal_Floating_Point_Definitions
            .Formal_Floating_Point_Definition'
            (Program.Nodes.Formal_Floating_Point_Definitions.Create
               (Digits_Token => Digits_Token, Box_Token => Box_Token));
   begin
      return Program.Elements.Formal_Floating_Point_Definitions
        .Formal_Floating_Point_Definition_Access
        (Result);
   end Create_Formal_Floating_Point_Definition;

   not overriding function Create_Formal_Ordinary_Fixed_Point_Definition
    (Self : Element_Factory;
     Delta_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Box_Token   : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Formal_Ordinary_Fixed_Point_Definitions
          .Formal_Ordinary_Fixed_Point_Definition_Access is
      Result : constant Formal_Ordinary_Fixed_Point_Definition_Access :=

          new (Self.Subpool) Program.Nodes
            .Formal_Ordinary_Fixed_Point_Definitions
            .Formal_Ordinary_Fixed_Point_Definition'
            (Program.Nodes.Formal_Ordinary_Fixed_Point_Definitions.Create
               (Delta_Token => Delta_Token, Box_Token => Box_Token));
   begin
      return Program.Elements.Formal_Ordinary_Fixed_Point_Definitions
        .Formal_Ordinary_Fixed_Point_Definition_Access
        (Result);
   end Create_Formal_Ordinary_Fixed_Point_Definition;

   not overriding function Create_Formal_Decimal_Fixed_Point_Definition
    (Self : Element_Factory;
     Delta_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Box_Token    : not null Program.Lexical_Elements.Lexical_Element_Access;
     Digits_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Box_Token_2  : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Formal_Decimal_Fixed_Point_Definitions
          .Formal_Decimal_Fixed_Point_Definition_Access is
      Result : constant Formal_Decimal_Fixed_Point_Definition_Access :=

          new (Self.Subpool) Program.Nodes
            .Formal_Decimal_Fixed_Point_Definitions
            .Formal_Decimal_Fixed_Point_Definition'
            (Program.Nodes.Formal_Decimal_Fixed_Point_Definitions.Create
               (Delta_Token => Delta_Token, Box_Token => Box_Token,
                Digits_Token => Digits_Token, Box_Token_2 => Box_Token_2));
   begin
      return Program.Elements.Formal_Decimal_Fixed_Point_Definitions
        .Formal_Decimal_Fixed_Point_Definition_Access
        (Result);
   end Create_Formal_Decimal_Fixed_Point_Definition;

   not overriding function Create_Range_Attribute_Reference
    (Self            : Element_Factory;
     Range_Attribute : not null Program.Elements.Attribute_References
         .Attribute_Reference_Access)
      return not null Program.Elements.Range_Attribute_References
          .Range_Attribute_Reference_Access is
      Result : constant Range_Attribute_Reference_Access :=

          new (Self.Subpool) Program.Nodes.Range_Attribute_References
            .Range_Attribute_Reference'
            (Program.Nodes.Range_Attribute_References.Create
               (Range_Attribute => Range_Attribute));
   begin
      return Program.Elements.Range_Attribute_References
        .Range_Attribute_Reference_Access
        (Result);
   end Create_Range_Attribute_Reference;

   not overriding function Create_Simple_Expression_Range
    (Self : Element_Factory;
     Lower_Bound      : not null Program.Elements.Expressions
         .Expression_Access;
     Double_Dot_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Upper_Bound      : not null Program.Elements.Expressions
         .Expression_Access)
      return not null Program.Elements.Simple_Expression_Ranges
          .Simple_Expression_Range_Access is
      Result : constant Simple_Expression_Range_Access :=

          new (Self.Subpool) Program.Nodes.Simple_Expression_Ranges
            .Simple_Expression_Range'
            (Program.Nodes.Simple_Expression_Ranges.Create
               (Lower_Bound => Lower_Bound,
                Double_Dot_Token => Double_Dot_Token,
                Upper_Bound => Upper_Bound));
   begin
      return Program.Elements.Simple_Expression_Ranges
        .Simple_Expression_Range_Access
        (Result);
   end Create_Simple_Expression_Range;

   not overriding function Create_Digits_Constraint
    (Self : Element_Factory;
     Digits_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Digits_Expression     : not null Program.Elements.Expressions
         .Expression_Access;
     Range_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Real_Range_Constraint : Program.Elements.Constraints.Constraint_Access)
      return not null Program.Elements.Digits_Constraints
          .Digits_Constraint_Access is
      Result : constant Digits_Constraint_Access :=

          new (Self.Subpool) Program.Nodes.Digits_Constraints
            .Digits_Constraint'
            (Program.Nodes.Digits_Constraints.Create
               (Digits_Token => Digits_Token,
                Digits_Expression => Digits_Expression,
                Range_Token => Range_Token,
                Real_Range_Constraint => Real_Range_Constraint));
   begin
      return Program.Elements.Digits_Constraints.Digits_Constraint_Access
        (Result);
   end Create_Digits_Constraint;

   not overriding function Create_Delta_Constraint
    (Self : Element_Factory;
     Delta_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Delta_Expression      : not null Program.Elements.Expressions
         .Expression_Access;
     Range_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Real_Range_Constraint : Program.Elements.Constraints.Constraint_Access)
      return not null Program.Elements.Delta_Constraints
          .Delta_Constraint_Access is
      Result : constant Delta_Constraint_Access :=

          new (Self.Subpool) Program.Nodes.Delta_Constraints.Delta_Constraint'
            (Program.Nodes.Delta_Constraints.Create
               (Delta_Token => Delta_Token,
                Delta_Expression => Delta_Expression,
                Range_Token => Range_Token,
                Real_Range_Constraint => Real_Range_Constraint));
   begin
      return Program.Elements.Delta_Constraints.Delta_Constraint_Access
        (Result);
   end Create_Delta_Constraint;

   not overriding function Create_Index_Constraint
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Ranges              : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Index_Constraints
          .Index_Constraint_Access is
      Result : constant Index_Constraint_Access :=

          new (Self.Subpool) Program.Nodes.Index_Constraints.Index_Constraint'
            (Program.Nodes.Index_Constraints.Create
               (Left_Bracket_Token => Left_Bracket_Token, Ranges => Ranges,
                Right_Bracket_Token => Right_Bracket_Token));
   begin
      return Program.Elements.Index_Constraints.Index_Constraint_Access
        (Result);
   end Create_Index_Constraint;

   not overriding function Create_Discriminant_Constraint
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Discriminants       : not null Program.Elements.Discriminant_Associations
         .Discriminant_Association_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Discriminant_Constraints
          .Discriminant_Constraint_Access is
      Result : constant Discriminant_Constraint_Access :=

          new (Self.Subpool) Program.Nodes.Discriminant_Constraints
            .Discriminant_Constraint'
            (Program.Nodes.Discriminant_Constraints.Create
               (Left_Bracket_Token => Left_Bracket_Token,
                Discriminants => Discriminants,
                Right_Bracket_Token => Right_Bracket_Token));
   begin
      return Program.Elements.Discriminant_Constraints
        .Discriminant_Constraint_Access
        (Result);
   end Create_Discriminant_Constraint;

   not overriding function Create_Attribute_Definition_Clause
    (Self : Element_Factory;
     For_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Expressions.Expression_Access;
     Use_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression      : not null Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Attribute_Definition_Clauses
          .Attribute_Definition_Clause_Access is
      Result : constant Attribute_Definition_Clause_Access :=

          new (Self.Subpool) Program.Nodes.Attribute_Definition_Clauses
            .Attribute_Definition_Clause'
            (Program.Nodes.Attribute_Definition_Clauses.Create
               (For_Token => For_Token, Name => Name, Use_Token => Use_Token,
                Expression => Expression, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Attribute_Definition_Clauses
        .Attribute_Definition_Clause_Access
        (Result);
   end Create_Attribute_Definition_Clause;

   not overriding function Create_Enumeration_Representation_Clause
    (Self : Element_Factory;
     For_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Expressions.Expression_Access;
     Use_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression      : not null Program.Elements.Array_Aggregates
         .Array_Aggregate_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Enumeration_Representation_Clauses
          .Enumeration_Representation_Clause_Access is
      Result : constant Enumeration_Representation_Clause_Access :=

          new (Self.Subpool) Program.Nodes.Enumeration_Representation_Clauses
            .Enumeration_Representation_Clause'
            (Program.Nodes.Enumeration_Representation_Clauses.Create
               (For_Token => For_Token, Name => Name, Use_Token => Use_Token,
                Expression => Expression, Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Enumeration_Representation_Clauses
        .Enumeration_Representation_Clause_Access
        (Result);
   end Create_Enumeration_Representation_Clause;

   not overriding function Create_Record_Representation_Clause
    (Self : Element_Factory;
     For_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                  : not null Program.Elements.Expressions
         .Expression_Access;
     Use_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Record_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     At_Token              : Program.Lexical_Elements.Lexical_Element_Access;
     Mod_Token             : Program.Lexical_Elements.Lexical_Element_Access;
     Mod_Clause_Expression : Program.Elements.Expressions.Expression_Access;
     Mod_Semicolon_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Component_Clauses     : not null Program.Elements.Component_Clauses
         .Component_Clause_Vector_Access;
     Semicolon_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Record_Representation_Clauses
          .Record_Representation_Clause_Access is
      Result : constant Record_Representation_Clause_Access :=

          new (Self.Subpool) Program.Nodes.Record_Representation_Clauses
            .Record_Representation_Clause'
            (Program.Nodes.Record_Representation_Clauses.Create
               (For_Token => For_Token, Name => Name, Use_Token => Use_Token,
                Record_Token => Record_Token, At_Token => At_Token,
                Mod_Token => Mod_Token,
                Mod_Clause_Expression => Mod_Clause_Expression,
                Mod_Semicolon_Token => Mod_Semicolon_Token,
                Component_Clauses => Component_Clauses,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.Record_Representation_Clauses
        .Record_Representation_Clause_Access
        (Result);
   end Create_Record_Representation_Clause;

   not overriding function Create_At_Clause
    (Self : Element_Factory;
     For_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Identifiers.Identifier_Access;
     Use_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     At_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression      : not null Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.At_Clauses.At_Clause_Access is
      Result : constant At_Clause_Access :=

          new (Self.Subpool) Program.Nodes.At_Clauses.At_Clause'
            (Program.Nodes.At_Clauses.Create
               (For_Token => For_Token, Name => Name, Use_Token => Use_Token,
                At_Token => At_Token, Expression => Expression,
                Semicolon_Token => Semicolon_Token));
   begin
      return Program.Elements.At_Clauses.At_Clause_Access (Result);
   end Create_At_Clause;

   not overriding function Create_Exception_Handler
    (Self : Element_Factory;
     When_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Choice_Parameter : Program.Elements.Choice_Parameter_Specifications
         .Choice_Parameter_Specification_Access;
     Choices          : not null Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements       : not null Program.Element_Vectors.Element_Vector_Access)
      return not null Program.Elements.Exception_Handlers
          .Exception_Handler_Access is
      Result : constant Exception_Handler_Access :=

          new (Self.Subpool) Program.Nodes.Exception_Handlers
            .Exception_Handler'
            (Program.Nodes.Exception_Handlers.Create
               (When_Token => When_Token, Choice_Parameter => Choice_Parameter,
                Choices => Choices, Arrow_Token => Arrow_Token,
                Statements => Statements));
   begin
      return Program.Elements.Exception_Handlers.Exception_Handler_Access
        (Result);
   end Create_Exception_Handler;

end Program.Element_Factories;
