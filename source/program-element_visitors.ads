--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Pragmas;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Defining_Character_Literals;
with Program.Elements.Defining_Operator_Symbols;
with Program.Elements.Defining_Expanded_Names;
with Program.Elements.Type_Declarations;
with Program.Elements.Task_Type_Declarations;
with Program.Elements.Protected_Type_Declarations;
with Program.Elements.Subtype_Declarations;
with Program.Elements.Object_Declarations;
with Program.Elements.Single_Task_Declarations;
with Program.Elements.Single_Protected_Declarations;
with Program.Elements.Number_Declarations;
with Program.Elements.Enumeration_Literal_Specifications;
with Program.Elements.Discriminant_Specifications;
with Program.Elements.Component_Declarations;
with Program.Elements.Loop_Parameter_Specifications;
with Program.Elements.Generalized_Iterator_Specifications;
with Program.Elements.Element_Iterator_Specifications;
with Program.Elements.Procedure_Declarations;
with Program.Elements.Function_Declarations;
with Program.Elements.Parameter_Specifications;
with Program.Elements.Procedure_Body_Declarations;
with Program.Elements.Function_Body_Declarations;
with Program.Elements.Return_Object_Specifications;
with Program.Elements.Package_Declarations;
with Program.Elements.Package_Body_Declarations;
with Program.Elements.Object_Renaming_Declarations;
with Program.Elements.Exception_Renaming_Declarations;
with Program.Elements.Procedure_Renaming_Declarations;
with Program.Elements.Function_Renaming_Declarations;
with Program.Elements.Package_Renaming_Declarations;
with Program.Elements.Generic_Package_Renaming_Declarations;
with Program.Elements.Generic_Procedure_Renaming_Declarations;
with Program.Elements.Generic_Function_Renaming_Declarations;
with Program.Elements.Task_Body_Declarations;
with Program.Elements.Protected_Body_Declarations;
with Program.Elements.Entry_Declarations;
with Program.Elements.Entry_Body_Declarations;
with Program.Elements.Entry_Index_Specifications;
with Program.Elements.Procedure_Body_Stubs;
with Program.Elements.Function_Body_Stubs;
with Program.Elements.Package_Body_Stubs;
with Program.Elements.Task_Body_Stubs;
with Program.Elements.Protected_Body_Stubs;
with Program.Elements.Exception_Declarations;
with Program.Elements.Choice_Parameter_Specifications;
with Program.Elements.Generic_Package_Declarations;
with Program.Elements.Generic_Procedure_Declarations;
with Program.Elements.Generic_Function_Declarations;
with Program.Elements.Package_Instantiations;
with Program.Elements.Procedure_Instantiations;
with Program.Elements.Function_Instantiations;
with Program.Elements.Formal_Object_Declarations;
with Program.Elements.Formal_Type_Declarations;
with Program.Elements.Formal_Procedure_Declarations;
with Program.Elements.Formal_Function_Declarations;
with Program.Elements.Formal_Package_Declarations;
with Program.Elements.Definitions;
with Program.Elements.Subtype_Indications;
with Program.Elements.Component_Definitions;
with Program.Elements.Unknown_Discriminant_Parts;
with Program.Elements.Known_Discriminant_Parts;
with Program.Elements.Record_Definitions;
with Program.Elements.Null_Components;
with Program.Elements.Variant_Parts;
with Program.Elements.Variants;
with Program.Elements.Others_Choices;
with Program.Elements.Private_Type_Definitions;
with Program.Elements.Private_Extension_Definitions;
with Program.Elements.Incomplete_Type_Definitions;
with Program.Elements.Task_Definitions;
with Program.Elements.Protected_Definitions;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Real_Range_Specifications;
with Program.Elements.Numeric_Literals;
with Program.Elements.String_Literals;
with Program.Elements.Identifiers;
with Program.Elements.Operator_Symbols;
with Program.Elements.Character_Literals;
with Program.Elements.Explicit_Dereferences;
with Program.Elements.Function_Calls;
with Program.Elements.Indexed_Components;
with Program.Elements.Slices;
with Program.Elements.Selected_Components;
with Program.Elements.Attribute_References;
with Program.Elements.Record_Aggregates;
with Program.Elements.Extension_Aggregates;
with Program.Elements.Array_Aggregates;
with Program.Elements.Short_Circuit_Operations;
with Program.Elements.Membership_Tests;
with Program.Elements.Null_Literals;
with Program.Elements.Parenthesized_Expressions;
with Program.Elements.Raise_Expressions;
with Program.Elements.Type_Conversions;
with Program.Elements.Qualified_Expressions;
with Program.Elements.Allocators;
with Program.Elements.Case_Expressions;
with Program.Elements.If_Expressions;
with Program.Elements.Quantified_Expressions;
with Program.Elements.Associations;
with Program.Elements.Discriminant_Associations;
with Program.Elements.Record_Component_Associations;
with Program.Elements.Array_Component_Associations;
with Program.Elements.Parameter_Associations;
with Program.Elements.Formal_Package_Associations;
with Program.Elements.Null_Statements;
with Program.Elements.Assignment_Statements;
with Program.Elements.If_Statements;
with Program.Elements.Case_Statements;
with Program.Elements.Loop_Statements;
with Program.Elements.While_Loop_Statements;
with Program.Elements.For_Loop_Statements;
with Program.Elements.Block_Statements;
with Program.Elements.Exit_Statements;
with Program.Elements.Goto_Statements;
with Program.Elements.Call_Statements;
with Program.Elements.Simple_Return_Statements;
with Program.Elements.Extended_Return_Statements;
with Program.Elements.Accept_Statements;
with Program.Elements.Requeue_Statements;
with Program.Elements.Delay_Statements;
with Program.Elements.Terminate_Alternative_Statements;
with Program.Elements.Select_Statements;
with Program.Elements.Abort_Statements;
with Program.Elements.Raise_Statements;
with Program.Elements.Code_Statements;
with Program.Elements.Paths;
with Program.Elements.Elsif_Paths;
with Program.Elements.Case_Paths;
with Program.Elements.Select_Paths;
with Program.Elements.Case_Expression_Paths;
with Program.Elements.Elsif_Expression_Paths;
with Program.Elements.Clauses;
with Program.Elements.Use_Clauses;
with Program.Elements.With_Clauses;
with Program.Elements.Component_Clauses;
with Program.Elements.Derived_Types;
with Program.Elements.Derived_Record_Extensions;
with Program.Elements.Enumeration_Types;
with Program.Elements.Signed_Integer_Types;
with Program.Elements.Modular_Types;
with Program.Elements.Root_Types;
with Program.Elements.Floating_Point_Types;
with Program.Elements.Ordinary_Fixed_Point_Types;
with Program.Elements.Decimal_Fixed_Point_Types;
with Program.Elements.Unconstrained_Array_Types;
with Program.Elements.Constrained_Array_Types;
with Program.Elements.Record_Types;
with Program.Elements.Interface_Types;
with Program.Elements.Object_Access_Types;
with Program.Elements.Procedure_Access_Types;
with Program.Elements.Function_Access_Types;
with Program.Elements.Formal_Private_Type_Definitions;
with Program.Elements.Formal_Derived_Type_Definitions;
with Program.Elements.Formal_Discrete_Type_Definitions;
with Program.Elements.Formal_Signed_Integer_Type_Definitions;
with Program.Elements.Formal_Modular_Type_Definitions;
with Program.Elements.Formal_Floating_Point_Definitions;
with Program.Elements.Formal_Ordinary_Fixed_Point_Definitions;
with Program.Elements.Formal_Decimal_Fixed_Point_Definitions;
with Program.Elements.Range_Attribute_References;
with Program.Elements.Simple_Expression_Ranges;
with Program.Elements.Digits_Constraints;
with Program.Elements.Delta_Constraints;
with Program.Elements.Index_Constraints;
with Program.Elements.Discriminant_Constraints;
with Program.Elements.Attribute_Definition_Clauses;
with Program.Elements.Enumeration_Representation_Clauses;
with Program.Elements.Record_Representation_Clauses;
with Program.Elements.At_Clauses;
with Program.Elements.Exception_Handlers;

package Program.Element_Visitors is

   pragma Pure (Program.Element_Visitors);

   type Element_Visitor is limited interface;

   procedure Pragma_Element
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Pragmas.Pragma_Access) is null;

   procedure Defining_Identifier
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access) is null;

   procedure Defining_Character_Literal
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Defining_Character_Literals
         .Defining_Character_Literal_Access) is null;

   procedure Defining_Operator_Symbol
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Defining_Operator_Symbols
         .Defining_Operator_Symbol_Access) is null;

   procedure Defining_Expanded_Name
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Defining_Expanded_Names
         .Defining_Expanded_Name_Access) is null;

   procedure Type_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Type_Declarations
         .Type_Declaration_Access) is null;

   procedure Task_Type_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Task_Type_Declarations
         .Task_Type_Declaration_Access) is null;

   procedure Protected_Type_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Protected_Type_Declarations
         .Protected_Type_Declaration_Access) is null;

   procedure Subtype_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Subtype_Declarations
         .Subtype_Declaration_Access) is null;

   procedure Object_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Object_Declarations
         .Object_Declaration_Access) is null;

   procedure Single_Task_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Single_Task_Declarations
         .Single_Task_Declaration_Access) is null;

   procedure Single_Protected_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Single_Protected_Declarations
         .Single_Protected_Declaration_Access) is null;

   procedure Number_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Number_Declarations
         .Number_Declaration_Access) is null;

   procedure Enumeration_Literal_Specification
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Enumeration_Literal_Specifications
         .Enumeration_Literal_Specification_Access) is null;

   procedure Discriminant_Specification
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Discriminant_Specifications
         .Discriminant_Specification_Access) is null;

   procedure Component_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Component_Declarations
         .Component_Declaration_Access) is null;

   procedure Loop_Parameter_Specification
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Loop_Parameter_Specifications
         .Loop_Parameter_Specification_Access) is null;

   procedure Generalized_Iterator_Specification
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Generalized_Iterator_Specifications
         .Generalized_Iterator_Specification_Access) is null;

   procedure Element_Iterator_Specification
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Element_Iterator_Specifications
         .Element_Iterator_Specification_Access) is null;

   procedure Procedure_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Procedure_Declarations
         .Procedure_Declaration_Access) is null;

   procedure Function_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Function_Declarations
         .Function_Declaration_Access) is null;

   procedure Parameter_Specification
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Access) is null;

   procedure Procedure_Body_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Procedure_Body_Declarations
         .Procedure_Body_Declaration_Access) is null;

   procedure Function_Body_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Function_Body_Declarations
         .Function_Body_Declaration_Access) is null;

   procedure Return_Object_Specification
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Return_Object_Specifications
         .Return_Object_Specification_Access) is null;

   procedure Package_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Package_Declarations
         .Package_Declaration_Access) is null;

   procedure Package_Body_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Package_Body_Declarations
         .Package_Body_Declaration_Access) is null;

   procedure Object_Renaming_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Object_Renaming_Declarations
         .Object_Renaming_Declaration_Access) is null;

   procedure Exception_Renaming_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Exception_Renaming_Declarations
         .Exception_Renaming_Declaration_Access) is null;

   procedure Procedure_Renaming_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Procedure_Renaming_Declarations
         .Procedure_Renaming_Declaration_Access) is null;

   procedure Function_Renaming_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Function_Renaming_Declarations
         .Function_Renaming_Declaration_Access) is null;

   procedure Package_Renaming_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Package_Renaming_Declarations
         .Package_Renaming_Declaration_Access) is null;

   procedure Generic_Package_Renaming_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Generic_Package_Renaming_Declarations
         .Generic_Package_Renaming_Declaration_Access) is null;

   procedure Generic_Procedure_Renaming_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements
         .Generic_Procedure_Renaming_Declarations
         .Generic_Procedure_Renaming_Declaration_Access) is null;

   procedure Generic_Function_Renaming_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Generic_Function_Renaming_Declarations
         .Generic_Function_Renaming_Declaration_Access) is null;

   procedure Task_Body_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Task_Body_Declarations
         .Task_Body_Declaration_Access) is null;

   procedure Protected_Body_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Protected_Body_Declarations
         .Protected_Body_Declaration_Access) is null;

   procedure Entry_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Entry_Declarations
         .Entry_Declaration_Access) is null;

   procedure Entry_Body_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Entry_Body_Declarations
         .Entry_Body_Declaration_Access) is null;

   procedure Entry_Index_Specification
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Entry_Index_Specifications
         .Entry_Index_Specification_Access) is null;

   procedure Procedure_Body_Stub
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Procedure_Body_Stubs
         .Procedure_Body_Stub_Access) is null;

   procedure Function_Body_Stub
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Function_Body_Stubs
         .Function_Body_Stub_Access) is null;

   procedure Package_Body_Stub
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Package_Body_Stubs
         .Package_Body_Stub_Access) is null;

   procedure Task_Body_Stub
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Task_Body_Stubs.Task_Body_Stub_Access)
     is null;

   procedure Protected_Body_Stub
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Protected_Body_Stubs
         .Protected_Body_Stub_Access) is null;

   procedure Exception_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Exception_Declarations
         .Exception_Declaration_Access) is null;

   procedure Choice_Parameter_Specification
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Choice_Parameter_Specifications
         .Choice_Parameter_Specification_Access) is null;

   procedure Generic_Package_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Generic_Package_Declarations
         .Generic_Package_Declaration_Access) is null;

   procedure Generic_Procedure_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Generic_Procedure_Declarations
         .Generic_Procedure_Declaration_Access) is null;

   procedure Generic_Function_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Generic_Function_Declarations
         .Generic_Function_Declaration_Access) is null;

   procedure Package_Instantiation
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Package_Instantiations
         .Package_Instantiation_Access) is null;

   procedure Procedure_Instantiation
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Procedure_Instantiations
         .Procedure_Instantiation_Access) is null;

   procedure Function_Instantiation
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Function_Instantiations
         .Function_Instantiation_Access) is null;

   procedure Formal_Object_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Formal_Object_Declarations
         .Formal_Object_Declaration_Access) is null;

   procedure Formal_Type_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Formal_Type_Declarations
         .Formal_Type_Declaration_Access) is null;

   procedure Formal_Procedure_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Formal_Procedure_Declarations
         .Formal_Procedure_Declaration_Access) is null;

   procedure Formal_Function_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Formal_Function_Declarations
         .Formal_Function_Declaration_Access) is null;

   procedure Formal_Package_Declaration
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Formal_Package_Declarations
         .Formal_Package_Declaration_Access) is null;

   procedure Definition
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Definitions.Definition_Access)
     is null;

   procedure Subtype_Indication
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access) is null;

   procedure Component_Definition
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Component_Definitions
         .Component_Definition_Access) is null;

   procedure Unknown_Discriminant_Part
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Unknown_Discriminant_Parts
         .Unknown_Discriminant_Part_Access) is null;

   procedure Known_Discriminant_Part
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Known_Discriminant_Parts
         .Known_Discriminant_Part_Access) is null;

   procedure Record_Definition
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Record_Definitions
         .Record_Definition_Access) is null;

   procedure Null_Component
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Null_Components.Null_Component_Access)
     is null;

   procedure Variant_Part
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Variant_Parts.Variant_Part_Access)
     is null;

   procedure Variant
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Variants.Variant_Access) is null;

   procedure Others_Choice
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Others_Choices.Others_Choice_Access)
     is null;

   procedure Private_Type_Definition
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Private_Type_Definitions
         .Private_Type_Definition_Access) is null;

   procedure Private_Extension_Definition
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Private_Extension_Definitions
         .Private_Extension_Definition_Access) is null;

   procedure Incomplete_Type_Definition
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Incomplete_Type_Definitions
         .Incomplete_Type_Definition_Access) is null;

   procedure Task_Definition
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Task_Definitions
         .Task_Definition_Access) is null;

   procedure Protected_Definition
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Protected_Definitions
         .Protected_Definition_Access) is null;

   procedure Aspect_Specification
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Access) is null;

   procedure Real_Range_Specification
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access) is null;

   procedure Numeric_Literal
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Numeric_Literals
         .Numeric_Literal_Access) is null;

   procedure String_Literal
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.String_Literals.String_Literal_Access)
     is null;

   procedure Identifier
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Identifiers.Identifier_Access)
     is null;

   procedure Operator_Symbol
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Operator_Symbols
         .Operator_Symbol_Access) is null;

   procedure Character_Literal
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Character_Literals
         .Character_Literal_Access) is null;

   procedure Explicit_Dereference
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Explicit_Dereferences
         .Explicit_Dereference_Access) is null;

   procedure Function_Call
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Function_Calls.Function_Call_Access)
     is null;

   procedure Indexed_Component
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Indexed_Components
         .Indexed_Component_Access) is null;

   procedure Slice
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Slices.Slice_Access) is null;

   procedure Selected_Component
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Selected_Components
         .Selected_Component_Access) is null;

   procedure Attribute_Reference
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Attribute_References
         .Attribute_Reference_Access) is null;

   procedure Record_Aggregate
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Record_Aggregates
         .Record_Aggregate_Access) is null;

   procedure Extension_Aggregate
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Extension_Aggregates
         .Extension_Aggregate_Access) is null;

   procedure Array_Aggregate
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Array_Aggregates
         .Array_Aggregate_Access) is null;

   procedure Short_Circuit_Operation
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Short_Circuit_Operations
         .Short_Circuit_Operation_Access) is null;

   procedure Membership_Test
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Membership_Tests
         .Membership_Test_Access) is null;

   procedure Null_Literal
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Null_Literals.Null_Literal_Access)
     is null;

   procedure Parenthesized_Expression
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Parenthesized_Expressions
         .Parenthesized_Expression_Access) is null;

   procedure Raise_Expression
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Raise_Expressions
         .Raise_Expression_Access) is null;

   procedure Type_Conversion
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Type_Conversions
         .Type_Conversion_Access) is null;

   procedure Qualified_Expression
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Qualified_Expressions
         .Qualified_Expression_Access) is null;

   procedure Allocator
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Allocators.Allocator_Access) is null;

   procedure Case_Expression
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Case_Expressions
         .Case_Expression_Access) is null;

   procedure If_Expression
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.If_Expressions.If_Expression_Access)
     is null;

   procedure Quantified_Expression
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Quantified_Expressions
         .Quantified_Expression_Access) is null;

   procedure Association
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Associations.Association_Access)
     is null;

   procedure Discriminant_Association
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Discriminant_Associations
         .Discriminant_Association_Access) is null;

   procedure Record_Component_Association
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Record_Component_Associations
         .Record_Component_Association_Access) is null;

   procedure Array_Component_Association
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Array_Component_Associations
         .Array_Component_Association_Access) is null;

   procedure Parameter_Association
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Access) is null;

   procedure Formal_Package_Association
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Formal_Package_Associations
         .Formal_Package_Association_Access) is null;

   procedure Null_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Null_Statements.Null_Statement_Access)
     is null;

   procedure Assignment_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Assignment_Statements
         .Assignment_Statement_Access) is null;

   procedure If_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.If_Statements.If_Statement_Access)
     is null;

   procedure Case_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Case_Statements.Case_Statement_Access)
     is null;

   procedure Loop_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Loop_Statements.Loop_Statement_Access)
     is null;

   procedure While_Loop_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.While_Loop_Statements
         .While_Loop_Statement_Access) is null;

   procedure For_Loop_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.For_Loop_Statements
         .For_Loop_Statement_Access) is null;

   procedure Block_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Block_Statements
         .Block_Statement_Access) is null;

   procedure Exit_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Exit_Statements.Exit_Statement_Access)
     is null;

   procedure Goto_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Goto_Statements.Goto_Statement_Access)
     is null;

   procedure Call_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Call_Statements.Call_Statement_Access)
     is null;

   procedure Simple_Return_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Simple_Return_Statements
         .Simple_Return_Statement_Access) is null;

   procedure Extended_Return_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Extended_Return_Statements
         .Extended_Return_Statement_Access) is null;

   procedure Accept_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Accept_Statements
         .Accept_Statement_Access) is null;

   procedure Requeue_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Requeue_Statements
         .Requeue_Statement_Access) is null;

   procedure Delay_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Delay_Statements
         .Delay_Statement_Access) is null;

   procedure Terminate_Alternative_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Terminate_Alternative_Statements
         .Terminate_Alternative_Statement_Access) is null;

   procedure Select_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Select_Statements
         .Select_Statement_Access) is null;

   procedure Abort_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Abort_Statements
         .Abort_Statement_Access) is null;

   procedure Raise_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Raise_Statements
         .Raise_Statement_Access) is null;

   procedure Code_Statement
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Code_Statements.Code_Statement_Access)
     is null;

   procedure Path
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Paths.Path_Access) is null;

   procedure Elsif_Path
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Elsif_Paths.Elsif_Path_Access)
     is null;

   procedure Case_Path
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Case_Paths.Case_Path_Access) is null;

   procedure Select_Path
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Select_Paths.Select_Path_Access)
     is null;

   procedure Case_Expression_Path
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Case_Expression_Paths
         .Case_Expression_Path_Access) is null;

   procedure Elsif_Expression_Path
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Elsif_Expression_Paths
         .Elsif_Expression_Path_Access) is null;

   procedure Clause
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Clauses.Clause_Access) is null;

   procedure Use_Clause
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Use_Clauses.Use_Clause_Access)
     is null;

   procedure With_Clause
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.With_Clauses.With_Clause_Access)
     is null;

   procedure Component_Clause
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Component_Clauses
         .Component_Clause_Access) is null;

   procedure Derived_Type
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Derived_Types.Derived_Type_Access)
     is null;

   procedure Derived_Record_Extension
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Derived_Record_Extensions
         .Derived_Record_Extension_Access) is null;

   procedure Enumeration_Type
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Enumeration_Types
         .Enumeration_Type_Access) is null;

   procedure Signed_Integer_Type
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Signed_Integer_Types
         .Signed_Integer_Type_Access) is null;

   procedure Modular_Type
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Modular_Types.Modular_Type_Access)
     is null;

   procedure Root_Type
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Root_Types.Root_Type_Access) is null;

   procedure Floating_Point_Type
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Floating_Point_Types
         .Floating_Point_Type_Access) is null;

   procedure Ordinary_Fixed_Point_Type
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Ordinary_Fixed_Point_Types
         .Ordinary_Fixed_Point_Type_Access) is null;

   procedure Decimal_Fixed_Point_Type
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Decimal_Fixed_Point_Types
         .Decimal_Fixed_Point_Type_Access) is null;

   procedure Unconstrained_Array_Type
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Unconstrained_Array_Types
         .Unconstrained_Array_Type_Access) is null;

   procedure Constrained_Array_Type
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Constrained_Array_Types
         .Constrained_Array_Type_Access) is null;

   procedure Record_Type
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Record_Types.Record_Type_Access)
     is null;

   procedure Interface_Type
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Interface_Types.Interface_Type_Access)
     is null;

   procedure Object_Access_Type
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Object_Access_Types
         .Object_Access_Type_Access) is null;

   procedure Procedure_Access_Type
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Procedure_Access_Types
         .Procedure_Access_Type_Access) is null;

   procedure Function_Access_Type
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Function_Access_Types
         .Function_Access_Type_Access) is null;

   procedure Formal_Private_Type_Definition
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Formal_Private_Type_Definitions
         .Formal_Private_Type_Definition_Access) is null;

   procedure Formal_Derived_Type_Definition
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Formal_Derived_Type_Definitions
         .Formal_Derived_Type_Definition_Access) is null;

   procedure Formal_Discrete_Type_Definition
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Formal_Discrete_Type_Definitions
         .Formal_Discrete_Type_Definition_Access) is null;

   procedure Formal_Signed_Integer_Type_Definition
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Formal_Signed_Integer_Type_Definitions
         .Formal_Signed_Integer_Type_Definition_Access) is null;

   procedure Formal_Modular_Type_Definition
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Formal_Modular_Type_Definitions
         .Formal_Modular_Type_Definition_Access) is null;

   procedure Formal_Floating_Point_Definition
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Formal_Floating_Point_Definitions
         .Formal_Floating_Point_Definition_Access) is null;

   procedure Formal_Ordinary_Fixed_Point_Definition
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements
         .Formal_Ordinary_Fixed_Point_Definitions
         .Formal_Ordinary_Fixed_Point_Definition_Access) is null;

   procedure Formal_Decimal_Fixed_Point_Definition
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Formal_Decimal_Fixed_Point_Definitions
         .Formal_Decimal_Fixed_Point_Definition_Access) is null;

   procedure Range_Attribute_Reference
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Range_Attribute_References
         .Range_Attribute_Reference_Access) is null;

   procedure Simple_Expression_Range
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Simple_Expression_Ranges
         .Simple_Expression_Range_Access) is null;

   procedure Digits_Constraint
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Digits_Constraints
         .Digits_Constraint_Access) is null;

   procedure Delta_Constraint
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Delta_Constraints
         .Delta_Constraint_Access) is null;

   procedure Index_Constraint
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Index_Constraints
         .Index_Constraint_Access) is null;

   procedure Discriminant_Constraint
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Discriminant_Constraints
         .Discriminant_Constraint_Access) is null;

   procedure Attribute_Definition_Clause
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Attribute_Definition_Clauses
         .Attribute_Definition_Clause_Access) is null;

   procedure Enumeration_Representation_Clause
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Enumeration_Representation_Clauses
         .Enumeration_Representation_Clause_Access) is null;

   procedure Record_Representation_Clause
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Record_Representation_Clauses
         .Record_Representation_Clause_Access) is null;

   procedure At_Clause
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.At_Clauses.At_Clause_Access) is null;

   procedure Exception_Handler
    (Self    : in out Element_Visitor;
     Element : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Access) is null;

end Program.Element_Visitors;
