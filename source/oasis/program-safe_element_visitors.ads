--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Visitors;
with Program.Elements;

private with Program.Elements.Pragmas;
private with Program.Elements.Defining_Identifiers;
private with Program.Elements.Defining_Character_Literals;
private with Program.Elements.Defining_Operator_Symbols;
private with Program.Elements.Defining_Expanded_Names;
private with Program.Elements.Type_Declarations;
private with Program.Elements.Task_Type_Declarations;
private with Program.Elements.Protected_Type_Declarations;
private with Program.Elements.Subtype_Declarations;
private with Program.Elements.Object_Declarations;
private with Program.Elements.Single_Task_Declarations;
private with Program.Elements.Single_Protected_Declarations;
private with Program.Elements.Number_Declarations;
private with Program.Elements.Enumeration_Literal_Specifications;
private with Program.Elements.Discriminant_Specifications;
private with Program.Elements.Component_Declarations;
private with Program.Elements.Loop_Parameter_Specifications;
private with Program.Elements.Generalized_Iterator_Specifications;
private with Program.Elements.Element_Iterator_Specifications;
private with Program.Elements.Procedure_Declarations;
private with Program.Elements.Function_Declarations;
private with Program.Elements.Parameter_Specifications;
private with Program.Elements.Procedure_Body_Declarations;
private with Program.Elements.Function_Body_Declarations;
private with Program.Elements.Return_Object_Specifications;
private with Program.Elements.Package_Declarations;
private with Program.Elements.Package_Body_Declarations;
private with Program.Elements.Object_Renaming_Declarations;
private with Program.Elements.Exception_Renaming_Declarations;
private with Program.Elements.Procedure_Renaming_Declarations;
private with Program.Elements.Function_Renaming_Declarations;
private with Program.Elements.Package_Renaming_Declarations;
private with Program.Elements.Generic_Package_Renaming_Declarations;
private with Program.Elements.Generic_Procedure_Renaming_Declarations;
private with Program.Elements.Generic_Function_Renaming_Declarations;
private with Program.Elements.Task_Body_Declarations;
private with Program.Elements.Protected_Body_Declarations;
private with Program.Elements.Entry_Declarations;
private with Program.Elements.Entry_Body_Declarations;
private with Program.Elements.Entry_Index_Specifications;
private with Program.Elements.Procedure_Body_Stubs;
private with Program.Elements.Function_Body_Stubs;
private with Program.Elements.Package_Body_Stubs;
private with Program.Elements.Task_Body_Stubs;
private with Program.Elements.Protected_Body_Stubs;
private with Program.Elements.Exception_Declarations;
private with Program.Elements.Choice_Parameter_Specifications;
private with Program.Elements.Generic_Package_Declarations;
private with Program.Elements.Generic_Procedure_Declarations;
private with Program.Elements.Generic_Function_Declarations;
private with Program.Elements.Package_Instantiations;
private with Program.Elements.Procedure_Instantiations;
private with Program.Elements.Function_Instantiations;
private with Program.Elements.Formal_Object_Declarations;
private with Program.Elements.Formal_Type_Declarations;
private with Program.Elements.Formal_Procedure_Declarations;
private with Program.Elements.Formal_Function_Declarations;
private with Program.Elements.Formal_Package_Declarations;
private with Program.Elements.Subtype_Indications;
private with Program.Elements.Component_Definitions;
private with Program.Elements.Discrete_Subtype_Indications;
private with Program.Elements.Discrete_Range_Attribute_References;
private with Program.Elements.Discrete_Simple_Expression_Ranges;
private with Program.Elements.Unknown_Discriminant_Parts;
private with Program.Elements.Known_Discriminant_Parts;
private with Program.Elements.Record_Definitions;
private with Program.Elements.Null_Components;
private with Program.Elements.Variant_Parts;
private with Program.Elements.Variants;
private with Program.Elements.Others_Choices;
private with Program.Elements.Anonymous_Access_To_Objects;
private with Program.Elements.Anonymous_Access_To_Procedures;
private with Program.Elements.Anonymous_Access_To_Functions;
private with Program.Elements.Private_Type_Definitions;
private with Program.Elements.Private_Extension_Definitions;
private with Program.Elements.Incomplete_Type_Definitions;
private with Program.Elements.Task_Definitions;
private with Program.Elements.Protected_Definitions;
private with Program.Elements.Aspect_Specifications;
private with Program.Elements.Real_Range_Specifications;
private with Program.Elements.Numeric_Literals;
private with Program.Elements.String_Literals;
private with Program.Elements.Identifiers;
private with Program.Elements.Operator_Symbols;
private with Program.Elements.Character_Literals;
private with Program.Elements.Explicit_Dereferences;
private with Program.Elements.Infix_Operators;
private with Program.Elements.Function_Calls;
private with Program.Elements.Indexed_Components;
private with Program.Elements.Slices;
private with Program.Elements.Selected_Components;
private with Program.Elements.Attribute_References;
private with Program.Elements.Record_Aggregates;
private with Program.Elements.Extension_Aggregates;
private with Program.Elements.Array_Aggregates;
private with Program.Elements.Short_Circuit_Operations;
private with Program.Elements.Membership_Tests;
private with Program.Elements.Null_Literals;
private with Program.Elements.Parenthesized_Expressions;
private with Program.Elements.Raise_Expressions;
private with Program.Elements.Type_Conversions;
private with Program.Elements.Qualified_Expressions;
private with Program.Elements.Allocators;
private with Program.Elements.Case_Expressions;
private with Program.Elements.If_Expressions;
private with Program.Elements.Quantified_Expressions;
private with Program.Elements.Discriminant_Associations;
private with Program.Elements.Record_Component_Associations;
private with Program.Elements.Array_Component_Associations;
private with Program.Elements.Parameter_Associations;
private with Program.Elements.Formal_Package_Associations;
private with Program.Elements.Null_Statements;
private with Program.Elements.Assignment_Statements;
private with Program.Elements.If_Statements;
private with Program.Elements.Case_Statements;
private with Program.Elements.Loop_Statements;
private with Program.Elements.While_Loop_Statements;
private with Program.Elements.For_Loop_Statements;
private with Program.Elements.Block_Statements;
private with Program.Elements.Exit_Statements;
private with Program.Elements.Goto_Statements;
private with Program.Elements.Call_Statements;
private with Program.Elements.Simple_Return_Statements;
private with Program.Elements.Extended_Return_Statements;
private with Program.Elements.Accept_Statements;
private with Program.Elements.Requeue_Statements;
private with Program.Elements.Delay_Statements;
private with Program.Elements.Terminate_Alternative_Statements;
private with Program.Elements.Select_Statements;
private with Program.Elements.Abort_Statements;
private with Program.Elements.Raise_Statements;
private with Program.Elements.Code_Statements;
private with Program.Elements.Elsif_Paths;
private with Program.Elements.Case_Paths;
private with Program.Elements.Select_Paths;
private with Program.Elements.Case_Expression_Paths;
private with Program.Elements.Elsif_Expression_Paths;
private with Program.Elements.Use_Clauses;
private with Program.Elements.With_Clauses;
private with Program.Elements.Component_Clauses;
private with Program.Elements.Derived_Types;
private with Program.Elements.Derived_Record_Extensions;
private with Program.Elements.Enumeration_Types;
private with Program.Elements.Signed_Integer_Types;
private with Program.Elements.Modular_Types;
private with Program.Elements.Root_Types;
private with Program.Elements.Floating_Point_Types;
private with Program.Elements.Ordinary_Fixed_Point_Types;
private with Program.Elements.Decimal_Fixed_Point_Types;
private with Program.Elements.Unconstrained_Array_Types;
private with Program.Elements.Constrained_Array_Types;
private with Program.Elements.Record_Types;
private with Program.Elements.Interface_Types;
private with Program.Elements.Object_Access_Types;
private with Program.Elements.Procedure_Access_Types;
private with Program.Elements.Function_Access_Types;
private with Program.Elements.Formal_Private_Type_Definitions;
private with Program.Elements.Formal_Derived_Type_Definitions;
private with Program.Elements.Formal_Discrete_Type_Definitions;
private with Program.Elements.Formal_Signed_Integer_Type_Definitions;
private with Program.Elements.Formal_Modular_Type_Definitions;
private with Program.Elements.Formal_Floating_Point_Definitions;
private with Program.Elements.Formal_Ordinary_Fixed_Point_Definitions;
private with Program.Elements.Formal_Decimal_Fixed_Point_Definitions;
private with Program.Elements.Formal_Unconstrained_Array_Types;
private with Program.Elements.Formal_Constrained_Array_Types;
private with Program.Elements.Formal_Object_Access_Types;
private with Program.Elements.Formal_Procedure_Access_Types;
private with Program.Elements.Formal_Function_Access_Types;
private with Program.Elements.Formal_Interface_Types;
private with Program.Elements.Range_Attribute_References;
private with Program.Elements.Simple_Expression_Ranges;
private with Program.Elements.Digits_Constraints;
private with Program.Elements.Delta_Constraints;
private with Program.Elements.Index_Constraints;
private with Program.Elements.Discriminant_Constraints;
private with Program.Elements.Attribute_Definition_Clauses;
private with Program.Elements.Enumeration_Representation_Clauses;
private with Program.Elements.Record_Representation_Clauses;
private with Program.Elements.At_Clauses;
private with Program.Elements.Exception_Handlers;

package Program.Safe_Element_Visitors is

   pragma Pure;

   type Safe_Element_Visitor is limited
     new Program.Element_Visitors.Element_Visitor with
   record
      Failed : Boolean := False;
   end record;

   procedure Visit
     (Self    : in out Safe_Element_Visitor'Class;
      Element : not null access Program.Elements.Element'Class);

private

   overriding procedure Pragma_Element
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Pragmas.Pragma_Access);

   overriding procedure Defining_Identifier
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access);

   overriding procedure Defining_Character_Literal
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Defining_Character_Literals
         .Defining_Character_Literal_Access);

   overriding procedure Defining_Operator_Symbol
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Defining_Operator_Symbols
         .Defining_Operator_Symbol_Access);

   overriding procedure Defining_Expanded_Name
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Defining_Expanded_Names
         .Defining_Expanded_Name_Access);

   overriding procedure Type_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Type_Declarations
         .Type_Declaration_Access);

   overriding procedure Task_Type_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Task_Type_Declarations
         .Task_Type_Declaration_Access);

   overriding procedure Protected_Type_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Protected_Type_Declarations
         .Protected_Type_Declaration_Access);

   overriding procedure Subtype_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Subtype_Declarations
         .Subtype_Declaration_Access);

   overriding procedure Object_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Object_Declarations
         .Object_Declaration_Access);

   overriding procedure Single_Task_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Single_Task_Declarations
         .Single_Task_Declaration_Access);

   overriding procedure Single_Protected_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Single_Protected_Declarations
         .Single_Protected_Declaration_Access);

   overriding procedure Number_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Number_Declarations
         .Number_Declaration_Access);

   overriding procedure Enumeration_Literal_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Enumeration_Literal_Specifications
         .Enumeration_Literal_Specification_Access);

   overriding procedure Discriminant_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Discriminant_Specifications
         .Discriminant_Specification_Access);

   overriding procedure Component_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Component_Declarations
         .Component_Declaration_Access);

   overriding procedure Loop_Parameter_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Loop_Parameter_Specifications
         .Loop_Parameter_Specification_Access);

   overriding procedure Generalized_Iterator_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Generalized_Iterator_Specifications
         .Generalized_Iterator_Specification_Access);

   overriding procedure Element_Iterator_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Element_Iterator_Specifications
         .Element_Iterator_Specification_Access);

   overriding procedure Procedure_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Procedure_Declarations
         .Procedure_Declaration_Access);

   overriding procedure Function_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Function_Declarations
         .Function_Declaration_Access);

   overriding procedure Parameter_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Access);

   overriding procedure Procedure_Body_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Procedure_Body_Declarations
         .Procedure_Body_Declaration_Access);

   overriding procedure Function_Body_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Function_Body_Declarations
         .Function_Body_Declaration_Access);

   overriding procedure Return_Object_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Return_Object_Specifications
         .Return_Object_Specification_Access);

   overriding procedure Package_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Package_Declarations
         .Package_Declaration_Access);

   overriding procedure Package_Body_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Package_Body_Declarations
         .Package_Body_Declaration_Access);

   overriding procedure Object_Renaming_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Object_Renaming_Declarations
         .Object_Renaming_Declaration_Access);

   overriding procedure Exception_Renaming_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Exception_Renaming_Declarations
         .Exception_Renaming_Declaration_Access);

   overriding procedure Procedure_Renaming_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Procedure_Renaming_Declarations
         .Procedure_Renaming_Declaration_Access);

   overriding procedure Function_Renaming_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Function_Renaming_Declarations
         .Function_Renaming_Declaration_Access);

   overriding procedure Package_Renaming_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Package_Renaming_Declarations
         .Package_Renaming_Declaration_Access);

   overriding procedure Generic_Package_Renaming_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Generic_Package_Renaming_Declarations
         .Generic_Package_Renaming_Declaration_Access);

   overriding procedure Generic_Procedure_Renaming_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements
         .Generic_Procedure_Renaming_Declarations
         .Generic_Procedure_Renaming_Declaration_Access);

   overriding procedure Generic_Function_Renaming_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Generic_Function_Renaming_Declarations
         .Generic_Function_Renaming_Declaration_Access);

   overriding procedure Task_Body_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Task_Body_Declarations
         .Task_Body_Declaration_Access);

   overriding procedure Protected_Body_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Protected_Body_Declarations
         .Protected_Body_Declaration_Access);

   overriding procedure Entry_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Entry_Declarations
         .Entry_Declaration_Access);

   overriding procedure Entry_Body_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Entry_Body_Declarations
         .Entry_Body_Declaration_Access);

   overriding procedure Entry_Index_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Entry_Index_Specifications
         .Entry_Index_Specification_Access);

   overriding procedure Procedure_Body_Stub
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Procedure_Body_Stubs
         .Procedure_Body_Stub_Access);

   overriding procedure Function_Body_Stub
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Function_Body_Stubs
         .Function_Body_Stub_Access);

   overriding procedure Package_Body_Stub
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Package_Body_Stubs
         .Package_Body_Stub_Access);

   overriding procedure Task_Body_Stub
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Task_Body_Stubs.Task_Body_Stub_Access);

   overriding procedure Protected_Body_Stub
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Protected_Body_Stubs
         .Protected_Body_Stub_Access);

   overriding procedure Exception_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Exception_Declarations
         .Exception_Declaration_Access);

   overriding procedure Choice_Parameter_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Choice_Parameter_Specifications
         .Choice_Parameter_Specification_Access);

   overriding procedure Generic_Package_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Generic_Package_Declarations
         .Generic_Package_Declaration_Access);

   overriding procedure Generic_Procedure_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Generic_Procedure_Declarations
         .Generic_Procedure_Declaration_Access);

   overriding procedure Generic_Function_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Generic_Function_Declarations
         .Generic_Function_Declaration_Access);

   overriding procedure Package_Instantiation
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Package_Instantiations
         .Package_Instantiation_Access);

   overriding procedure Procedure_Instantiation
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Procedure_Instantiations
         .Procedure_Instantiation_Access);

   overriding procedure Function_Instantiation
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Function_Instantiations
         .Function_Instantiation_Access);

   overriding procedure Formal_Object_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Object_Declarations
         .Formal_Object_Declaration_Access);

   overriding procedure Formal_Type_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Type_Declarations
         .Formal_Type_Declaration_Access);

   overriding procedure Formal_Procedure_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Procedure_Declarations
         .Formal_Procedure_Declaration_Access);

   overriding procedure Formal_Function_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Function_Declarations
         .Formal_Function_Declaration_Access);

   overriding procedure Formal_Package_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Package_Declarations
         .Formal_Package_Declaration_Access);

   overriding procedure Subtype_Indication
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access);

   overriding procedure Component_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Component_Definitions
         .Component_Definition_Access);

   overriding procedure Discrete_Subtype_Indication
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Discrete_Subtype_Indications
         .Discrete_Subtype_Indication_Access);

   overriding procedure Discrete_Range_Attribute_Reference
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Discrete_Range_Attribute_References
         .Discrete_Range_Attribute_Reference_Access);

   overriding procedure Discrete_Simple_Expression_Range
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Discrete_Simple_Expression_Ranges
         .Discrete_Simple_Expression_Range_Access);

   overriding procedure Unknown_Discriminant_Part
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Unknown_Discriminant_Parts
         .Unknown_Discriminant_Part_Access);

   overriding procedure Known_Discriminant_Part
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Known_Discriminant_Parts
         .Known_Discriminant_Part_Access);

   overriding procedure Record_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Record_Definitions
         .Record_Definition_Access);

   overriding procedure Null_Component
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Null_Components.Null_Component_Access);

   overriding procedure Variant_Part
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Variant_Parts.Variant_Part_Access);

   overriding procedure Variant
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Variants.Variant_Access);

   overriding procedure Others_Choice
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Others_Choices.Others_Choice_Access);

   overriding procedure Anonymous_Access_To_Object
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Anonymous_Access_To_Objects
         .Anonymous_Access_To_Object_Access);

   overriding procedure Anonymous_Access_To_Procedure
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Anonymous_Access_To_Procedures
         .Anonymous_Access_To_Procedure_Access);

   overriding procedure Anonymous_Access_To_Function
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Anonymous_Access_To_Functions
         .Anonymous_Access_To_Function_Access);

   overriding procedure Private_Type_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Private_Type_Definitions
         .Private_Type_Definition_Access);

   overriding procedure Private_Extension_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Private_Extension_Definitions
         .Private_Extension_Definition_Access);

   overriding procedure Incomplete_Type_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Incomplete_Type_Definitions
         .Incomplete_Type_Definition_Access);

   overriding procedure Task_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Task_Definitions
         .Task_Definition_Access);

   overriding procedure Protected_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Protected_Definitions
         .Protected_Definition_Access);

   overriding procedure Aspect_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Access);

   overriding procedure Real_Range_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access);

   overriding procedure Numeric_Literal
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Numeric_Literals
         .Numeric_Literal_Access);

   overriding procedure String_Literal
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.String_Literals.String_Literal_Access);

   overriding procedure Identifier
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Identifiers.Identifier_Access);

   overriding procedure Operator_Symbol
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Operator_Symbols
         .Operator_Symbol_Access);

   overriding procedure Character_Literal
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Character_Literals
         .Character_Literal_Access);

   overriding procedure Explicit_Dereference
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Explicit_Dereferences
         .Explicit_Dereference_Access);

   overriding procedure Infix_Operator
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Infix_Operators.Infix_Operator_Access);

   overriding procedure Function_Call
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Function_Calls.Function_Call_Access);

   overriding procedure Indexed_Component
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Indexed_Components
         .Indexed_Component_Access);

   overriding procedure Slice
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Slices.Slice_Access);

   overriding procedure Selected_Component
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Selected_Components
         .Selected_Component_Access);

   overriding procedure Attribute_Reference
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Attribute_References
         .Attribute_Reference_Access);

   overriding procedure Record_Aggregate
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Record_Aggregates
         .Record_Aggregate_Access);

   overriding procedure Extension_Aggregate
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Extension_Aggregates
         .Extension_Aggregate_Access);

   overriding procedure Array_Aggregate
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Array_Aggregates
         .Array_Aggregate_Access);

   overriding procedure Short_Circuit_Operation
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Short_Circuit_Operations
         .Short_Circuit_Operation_Access);

   overriding procedure Membership_Test
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Membership_Tests
         .Membership_Test_Access);

   overriding procedure Null_Literal
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Null_Literals.Null_Literal_Access);

   overriding procedure Parenthesized_Expression
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Parenthesized_Expressions
         .Parenthesized_Expression_Access);

   overriding procedure Raise_Expression
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Raise_Expressions
         .Raise_Expression_Access);

   overriding procedure Type_Conversion
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Type_Conversions
         .Type_Conversion_Access);

   overriding procedure Qualified_Expression
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Qualified_Expressions
         .Qualified_Expression_Access);

   overriding procedure Allocator
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Allocators.Allocator_Access);

   overriding procedure Case_Expression
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Case_Expressions
         .Case_Expression_Access);

   overriding procedure If_Expression
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.If_Expressions.If_Expression_Access);

   overriding procedure Quantified_Expression
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Quantified_Expressions
         .Quantified_Expression_Access);

   overriding procedure Discriminant_Association
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Discriminant_Associations
         .Discriminant_Association_Access);

   overriding procedure Record_Component_Association
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Record_Component_Associations
         .Record_Component_Association_Access);

   overriding procedure Array_Component_Association
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Array_Component_Associations
         .Array_Component_Association_Access);

   overriding procedure Parameter_Association
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Access);

   overriding procedure Formal_Package_Association
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Package_Associations
         .Formal_Package_Association_Access);

   overriding procedure Null_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Null_Statements.Null_Statement_Access);

   overriding procedure Assignment_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Assignment_Statements
         .Assignment_Statement_Access);

   overriding procedure If_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.If_Statements.If_Statement_Access);

   overriding procedure Case_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Case_Statements.Case_Statement_Access);

   overriding procedure Loop_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Loop_Statements.Loop_Statement_Access);

   overriding procedure While_Loop_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.While_Loop_Statements
         .While_Loop_Statement_Access);

   overriding procedure For_Loop_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.For_Loop_Statements
         .For_Loop_Statement_Access);

   overriding procedure Block_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Block_Statements
         .Block_Statement_Access);

   overriding procedure Exit_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Exit_Statements.Exit_Statement_Access);

   overriding procedure Goto_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Goto_Statements.Goto_Statement_Access);

   overriding procedure Call_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Call_Statements.Call_Statement_Access);

   overriding procedure Simple_Return_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Simple_Return_Statements
         .Simple_Return_Statement_Access);

   overriding procedure Extended_Return_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Extended_Return_Statements
         .Extended_Return_Statement_Access);

   overriding procedure Accept_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Accept_Statements
         .Accept_Statement_Access);

   overriding procedure Requeue_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Requeue_Statements
         .Requeue_Statement_Access);

   overriding procedure Delay_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Delay_Statements
         .Delay_Statement_Access);

   overriding procedure Terminate_Alternative_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Terminate_Alternative_Statements
         .Terminate_Alternative_Statement_Access);

   overriding procedure Select_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Select_Statements
         .Select_Statement_Access);

   overriding procedure Abort_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Abort_Statements
         .Abort_Statement_Access);

   overriding procedure Raise_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Raise_Statements
         .Raise_Statement_Access);

   overriding procedure Code_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Code_Statements.Code_Statement_Access);

   overriding procedure Elsif_Path
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Elsif_Paths.Elsif_Path_Access);

   overriding procedure Case_Path
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Case_Paths.Case_Path_Access);

   overriding procedure Select_Path
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Select_Paths.Select_Path_Access);

   overriding procedure Case_Expression_Path
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Case_Expression_Paths
         .Case_Expression_Path_Access);

   overriding procedure Elsif_Expression_Path
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Elsif_Expression_Paths
         .Elsif_Expression_Path_Access);

   overriding procedure Use_Clause
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Use_Clauses.Use_Clause_Access);

   overriding procedure With_Clause
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.With_Clauses.With_Clause_Access);

   overriding procedure Component_Clause
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Component_Clauses
         .Component_Clause_Access);

   overriding procedure Derived_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Derived_Types.Derived_Type_Access);

   overriding procedure Derived_Record_Extension
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Derived_Record_Extensions
         .Derived_Record_Extension_Access);

   overriding procedure Enumeration_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Enumeration_Types
         .Enumeration_Type_Access);

   overriding procedure Signed_Integer_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Signed_Integer_Types
         .Signed_Integer_Type_Access);

   overriding procedure Modular_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Modular_Types.Modular_Type_Access);

   overriding procedure Root_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Root_Types.Root_Type_Access);

   overriding procedure Floating_Point_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Floating_Point_Types
         .Floating_Point_Type_Access);

   overriding procedure Ordinary_Fixed_Point_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Ordinary_Fixed_Point_Types
         .Ordinary_Fixed_Point_Type_Access);

   overriding procedure Decimal_Fixed_Point_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Decimal_Fixed_Point_Types
         .Decimal_Fixed_Point_Type_Access);

   overriding procedure Unconstrained_Array_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Unconstrained_Array_Types
         .Unconstrained_Array_Type_Access);

   overriding procedure Constrained_Array_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Constrained_Array_Types
         .Constrained_Array_Type_Access);

   overriding procedure Record_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Record_Types.Record_Type_Access);

   overriding procedure Interface_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Interface_Types.Interface_Type_Access);

   overriding procedure Object_Access_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Object_Access_Types
         .Object_Access_Type_Access);

   overriding procedure Procedure_Access_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Procedure_Access_Types
         .Procedure_Access_Type_Access);

   overriding procedure Function_Access_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Function_Access_Types
         .Function_Access_Type_Access);

   overriding procedure Formal_Private_Type_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Private_Type_Definitions
         .Formal_Private_Type_Definition_Access);

   overriding procedure Formal_Derived_Type_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Derived_Type_Definitions
         .Formal_Derived_Type_Definition_Access);

   overriding procedure Formal_Discrete_Type_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Discrete_Type_Definitions
         .Formal_Discrete_Type_Definition_Access);

   overriding procedure Formal_Signed_Integer_Type_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Signed_Integer_Type_Definitions
         .Formal_Signed_Integer_Type_Definition_Access);

   overriding procedure Formal_Modular_Type_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Modular_Type_Definitions
         .Formal_Modular_Type_Definition_Access);

   overriding procedure Formal_Floating_Point_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Floating_Point_Definitions
         .Formal_Floating_Point_Definition_Access);

   overriding procedure Formal_Ordinary_Fixed_Point_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements
         .Formal_Ordinary_Fixed_Point_Definitions
         .Formal_Ordinary_Fixed_Point_Definition_Access);

   overriding procedure Formal_Decimal_Fixed_Point_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Decimal_Fixed_Point_Definitions
         .Formal_Decimal_Fixed_Point_Definition_Access);

   overriding procedure Formal_Unconstrained_Array_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Unconstrained_Array_Types
         .Formal_Unconstrained_Array_Type_Access);

   overriding procedure Formal_Constrained_Array_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Constrained_Array_Types
         .Formal_Constrained_Array_Type_Access);

   overriding procedure Formal_Object_Access_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Object_Access_Types
         .Formal_Object_Access_Type_Access);

   overriding procedure Formal_Procedure_Access_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Procedure_Access_Types
         .Formal_Procedure_Access_Type_Access);

   overriding procedure Formal_Function_Access_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Function_Access_Types
         .Formal_Function_Access_Type_Access);

   overriding procedure Formal_Interface_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Interface_Types
         .Formal_Interface_Type_Access);

   overriding procedure Range_Attribute_Reference
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Range_Attribute_References
         .Range_Attribute_Reference_Access);

   overriding procedure Simple_Expression_Range
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Simple_Expression_Ranges
         .Simple_Expression_Range_Access);

   overriding procedure Digits_Constraint
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Digits_Constraints
         .Digits_Constraint_Access);

   overriding procedure Delta_Constraint
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Delta_Constraints
         .Delta_Constraint_Access);

   overriding procedure Index_Constraint
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Index_Constraints
         .Index_Constraint_Access);

   overriding procedure Discriminant_Constraint
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Discriminant_Constraints
         .Discriminant_Constraint_Access);

   overriding procedure Attribute_Definition_Clause
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Attribute_Definition_Clauses
         .Attribute_Definition_Clause_Access);

   overriding procedure Enumeration_Representation_Clause
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Enumeration_Representation_Clauses
         .Enumeration_Representation_Clause_Access);

   overriding procedure Record_Representation_Clause
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Record_Representation_Clauses
         .Record_Representation_Clause_Access);

   overriding procedure At_Clause
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.At_Clauses.At_Clause_Access);

   overriding procedure Exception_Handler
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Access);

end Program.Safe_Element_Visitors;
