--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;

with Program.Element_Visitors;

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
with Program.Elements.Subtype_Indications;
with Program.Elements.Component_Definitions;
with Program.Elements.Discrete_Subtype_Indications;
with Program.Elements.Discrete_Range_Attribute_References;
with Program.Elements.Discrete_Simple_Expression_Ranges;
with Program.Elements.Unknown_Discriminant_Parts;
with Program.Elements.Known_Discriminant_Parts;
with Program.Elements.Record_Definitions;
with Program.Elements.Null_Components;
with Program.Elements.Variant_Parts;
with Program.Elements.Variants;
with Program.Elements.Others_Choices;
with Program.Elements.Anonymous_Access_To_Objects;
with Program.Elements.Anonymous_Access_To_Procedures;
with Program.Elements.Anonymous_Access_To_Functions;
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
with Program.Elements.Infix_Operators;
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
with Program.Elements.Elsif_Paths;
with Program.Elements.Case_Paths;
with Program.Elements.Select_Paths;
with Program.Elements.Case_Expression_Paths;
with Program.Elements.Elsif_Expression_Paths;
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
with Program.Elements.Formal_Unconstrained_Array_Types;
with Program.Elements.Formal_Constrained_Array_Types;
with Program.Elements.Formal_Object_Access_Types;
with Program.Elements.Formal_Procedure_Access_Types;
with Program.Elements.Formal_Function_Access_Types;
with Program.Elements.Formal_Interface_Types;
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

package body Dump_Elements is

   package Visitors is
      type Visitor is new Program.Element_Visitors.Element_Visitor with record
         Level : Natural := 0;
      end record;

      procedure Pragma_Element
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Pragmas.Pragma_Access);

      procedure Defining_Identifier
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Defining_Identifiers
           .Defining_Identifier_Access);

      procedure Defining_Character_Literal
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Defining_Character_Literals
           .Defining_Character_Literal_Access);

      procedure Defining_Operator_Symbol
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Defining_Operator_Symbols
           .Defining_Operator_Symbol_Access);

      procedure Defining_Expanded_Name
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Defining_Expanded_Names
           .Defining_Expanded_Name_Access);

      procedure Type_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Type_Declarations
           .Type_Declaration_Access);

      procedure Task_Type_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Task_Type_Declarations
           .Task_Type_Declaration_Access);

      procedure Protected_Type_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Protected_Type_Declarations
           .Protected_Type_Declaration_Access);

      procedure Subtype_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Subtype_Declarations
           .Subtype_Declaration_Access);

      procedure Object_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Object_Declarations
           .Object_Declaration_Access);

      procedure Single_Task_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Single_Task_Declarations
           .Single_Task_Declaration_Access);

      procedure Single_Protected_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Single_Protected_Declarations
           .Single_Protected_Declaration_Access);

      procedure Number_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Number_Declarations
           .Number_Declaration_Access);

      procedure Enumeration_Literal_Specification
        (Self    : in out Visitor;
         Element : not null Program.Elements.Enumeration_Literal_Specifications
           .Enumeration_Literal_Specification_Access);

      procedure Discriminant_Specification
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Discriminant_Specifications
           .Discriminant_Specification_Access);

      procedure Component_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Component_Declarations
           .Component_Declaration_Access);

      procedure Loop_Parameter_Specification
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Loop_Parameter_Specifications
           .Loop_Parameter_Specification_Access);

      procedure Generalized_Iterator_Specification
        (Self    : in out Visitor;
         Element :        not null Program.Elements
           .Generalized_Iterator_Specifications
           .Generalized_Iterator_Specification_Access);

      procedure Element_Iterator_Specification
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Element_Iterator_Specifications
           .Element_Iterator_Specification_Access);

      procedure Procedure_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Procedure_Declarations
           .Procedure_Declaration_Access);

      procedure Function_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Function_Declarations
           .Function_Declaration_Access);

      procedure Parameter_Specification
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Parameter_Specifications
           .Parameter_Specification_Access);

      procedure Procedure_Body_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Procedure_Body_Declarations
           .Procedure_Body_Declaration_Access);

      procedure Function_Body_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Function_Body_Declarations
           .Function_Body_Declaration_Access);

      procedure Return_Object_Specification
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Return_Object_Specifications
           .Return_Object_Specification_Access);

      procedure Package_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Package_Declarations
           .Package_Declaration_Access);

      procedure Package_Body_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Package_Body_Declarations
           .Package_Body_Declaration_Access);

      procedure Object_Renaming_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Object_Renaming_Declarations
           .Object_Renaming_Declaration_Access);

      procedure Exception_Renaming_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Exception_Renaming_Declarations
           .Exception_Renaming_Declaration_Access);

      procedure Procedure_Renaming_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Procedure_Renaming_Declarations
           .Procedure_Renaming_Declaration_Access);

      procedure Function_Renaming_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Function_Renaming_Declarations
           .Function_Renaming_Declaration_Access);

      procedure Package_Renaming_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Package_Renaming_Declarations
           .Package_Renaming_Declaration_Access);

      procedure Generic_Package_Renaming_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements
           .Generic_Package_Renaming_Declarations
           .Generic_Package_Renaming_Declaration_Access);

      procedure Generic_Procedure_Renaming_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements
           .Generic_Procedure_Renaming_Declarations
           .Generic_Procedure_Renaming_Declaration_Access);

      procedure Generic_Function_Renaming_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements
           .Generic_Function_Renaming_Declarations
           .Generic_Function_Renaming_Declaration_Access);

      procedure Task_Body_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Task_Body_Declarations
           .Task_Body_Declaration_Access);

      procedure Protected_Body_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Protected_Body_Declarations
           .Protected_Body_Declaration_Access);

      procedure Entry_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Entry_Declarations
           .Entry_Declaration_Access);

      procedure Entry_Body_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Entry_Body_Declarations
           .Entry_Body_Declaration_Access);

      procedure Entry_Index_Specification
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Entry_Index_Specifications
           .Entry_Index_Specification_Access);

      procedure Procedure_Body_Stub
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Procedure_Body_Stubs
           .Procedure_Body_Stub_Access);

      procedure Function_Body_Stub
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Function_Body_Stubs
           .Function_Body_Stub_Access);

      procedure Package_Body_Stub
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Package_Body_Stubs
           .Package_Body_Stub_Access);

      procedure Task_Body_Stub
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Task_Body_Stubs
           .Task_Body_Stub_Access);

      procedure Protected_Body_Stub
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Protected_Body_Stubs
           .Protected_Body_Stub_Access);

      procedure Exception_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Exception_Declarations
           .Exception_Declaration_Access);

      procedure Choice_Parameter_Specification
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Choice_Parameter_Specifications
           .Choice_Parameter_Specification_Access);

      procedure Generic_Package_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Generic_Package_Declarations
           .Generic_Package_Declaration_Access);

      procedure Generic_Procedure_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Generic_Procedure_Declarations
           .Generic_Procedure_Declaration_Access);

      procedure Generic_Function_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Generic_Function_Declarations
           .Generic_Function_Declaration_Access);

      procedure Package_Instantiation
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Package_Instantiations
           .Package_Instantiation_Access);

      procedure Procedure_Instantiation
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Procedure_Instantiations
           .Procedure_Instantiation_Access);

      procedure Function_Instantiation
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Function_Instantiations
           .Function_Instantiation_Access);

      procedure Formal_Object_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Formal_Object_Declarations
           .Formal_Object_Declaration_Access);

      procedure Formal_Type_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Formal_Type_Declarations
           .Formal_Type_Declaration_Access);

      procedure Formal_Procedure_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Formal_Procedure_Declarations
           .Formal_Procedure_Declaration_Access);

      procedure Formal_Function_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Formal_Function_Declarations
           .Formal_Function_Declaration_Access);

      procedure Formal_Package_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Formal_Package_Declarations
           .Formal_Package_Declaration_Access);

      procedure Subtype_Indication
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Subtype_Indications
           .Subtype_Indication_Access);

      procedure Component_Definition
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Component_Definitions
           .Component_Definition_Access);

      procedure Discrete_Subtype_Indication
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Discrete_Subtype_Indications
           .Discrete_Subtype_Indication_Access);

      procedure Discrete_Range_Attribute_Reference
        (Self    : in out Visitor;
         Element :        not null Program.Elements
           .Discrete_Range_Attribute_References
           .Discrete_Range_Attribute_Reference_Access);

      procedure Discrete_Simple_Expression_Range
        (Self    : in out Visitor;
         Element : not null Program.Elements.Discrete_Simple_Expression_Ranges
           .Discrete_Simple_Expression_Range_Access);

      procedure Unknown_Discriminant_Part
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Unknown_Discriminant_Parts
           .Unknown_Discriminant_Part_Access);

      procedure Known_Discriminant_Part
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Known_Discriminant_Parts
           .Known_Discriminant_Part_Access);

      procedure Record_Definition
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Record_Definitions
           .Record_Definition_Access);

      procedure Null_Component
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Null_Components
           .Null_Component_Access);

      procedure Variant_Part
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Variant_Parts
           .Variant_Part_Access);

      procedure Variant
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Variants.Variant_Access);

      procedure Others_Choice
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Others_Choices
           .Others_Choice_Access);

      procedure Anonymous_Access_To_Object
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Anonymous_Access_To_Objects
           .Anonymous_Access_To_Object_Access);

      procedure Anonymous_Access_To_Procedure
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Anonymous_Access_To_Procedures
           .Anonymous_Access_To_Procedure_Access);

      procedure Anonymous_Access_To_Function
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Anonymous_Access_To_Functions
           .Anonymous_Access_To_Function_Access);

      procedure Private_Type_Definition
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Private_Type_Definitions
           .Private_Type_Definition_Access);

      procedure Private_Extension_Definition
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Private_Extension_Definitions
           .Private_Extension_Definition_Access);

      procedure Incomplete_Type_Definition
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Incomplete_Type_Definitions
           .Incomplete_Type_Definition_Access);

      procedure Task_Definition
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Task_Definitions
           .Task_Definition_Access);

      procedure Protected_Definition
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Protected_Definitions
           .Protected_Definition_Access);

      procedure Aspect_Specification
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Aspect_Specifications
           .Aspect_Specification_Access);

      procedure Real_Range_Specification
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Real_Range_Specifications
           .Real_Range_Specification_Access);

      procedure Numeric_Literal
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Numeric_Literals
           .Numeric_Literal_Access);

      procedure String_Literal
        (Self    : in out Visitor;
         Element :        not null Program.Elements.String_Literals
           .String_Literal_Access);

      procedure Identifier
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Identifiers.Identifier_Access);

      procedure Operator_Symbol
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Operator_Symbols
           .Operator_Symbol_Access);

      procedure Character_Literal
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Character_Literals
           .Character_Literal_Access);

      procedure Explicit_Dereference
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Explicit_Dereferences
           .Explicit_Dereference_Access);

      procedure Infix_Operator
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Infix_Operators
           .Infix_Operator_Access);

      procedure Function_Call
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Function_Calls
           .Function_Call_Access);

      procedure Indexed_Component
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Indexed_Components
           .Indexed_Component_Access);

      procedure Slice
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Slices.Slice_Access);

      procedure Selected_Component
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Selected_Components
           .Selected_Component_Access);

      procedure Attribute_Reference
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Attribute_References
           .Attribute_Reference_Access);

      procedure Record_Aggregate
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Record_Aggregates
           .Record_Aggregate_Access);

      procedure Extension_Aggregate
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Extension_Aggregates
           .Extension_Aggregate_Access);

      procedure Array_Aggregate
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Array_Aggregates
           .Array_Aggregate_Access);

      procedure Short_Circuit_Operation
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Short_Circuit_Operations
           .Short_Circuit_Operation_Access);

      procedure Membership_Test
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Membership_Tests
           .Membership_Test_Access);

      procedure Null_Literal
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Null_Literals
           .Null_Literal_Access);

      procedure Parenthesized_Expression
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Parenthesized_Expressions
           .Parenthesized_Expression_Access);

      procedure Raise_Expression
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Raise_Expressions
           .Raise_Expression_Access);

      procedure Type_Conversion
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Type_Conversions
           .Type_Conversion_Access);

      procedure Qualified_Expression
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Qualified_Expressions
           .Qualified_Expression_Access);

      procedure Allocator
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Allocators.Allocator_Access);

      procedure Case_Expression
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Case_Expressions
           .Case_Expression_Access);

      procedure If_Expression
        (Self    : in out Visitor;
         Element :        not null Program.Elements.If_Expressions
           .If_Expression_Access);

      procedure Quantified_Expression
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Quantified_Expressions
           .Quantified_Expression_Access);

      procedure Discriminant_Association
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Discriminant_Associations
           .Discriminant_Association_Access);

      procedure Record_Component_Association
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Record_Component_Associations
           .Record_Component_Association_Access);

      procedure Array_Component_Association
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Array_Component_Associations
           .Array_Component_Association_Access);

      procedure Parameter_Association
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Parameter_Associations
           .Parameter_Association_Access);

      procedure Formal_Package_Association
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Formal_Package_Associations
           .Formal_Package_Association_Access);

      procedure Null_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Null_Statements
           .Null_Statement_Access);

      procedure Assignment_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Assignment_Statements
           .Assignment_Statement_Access);

      procedure If_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.If_Statements
           .If_Statement_Access);

      procedure Case_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Case_Statements
           .Case_Statement_Access);

      procedure Loop_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Loop_Statements
           .Loop_Statement_Access);

      procedure While_Loop_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.While_Loop_Statements
           .While_Loop_Statement_Access);

      procedure For_Loop_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.For_Loop_Statements
           .For_Loop_Statement_Access);

      procedure Block_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Block_Statements
           .Block_Statement_Access);

      procedure Exit_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Exit_Statements
           .Exit_Statement_Access);

      procedure Goto_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Goto_Statements
           .Goto_Statement_Access);

      procedure Call_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Call_Statements
           .Call_Statement_Access);

      procedure Simple_Return_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Simple_Return_Statements
           .Simple_Return_Statement_Access);

      procedure Extended_Return_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Extended_Return_Statements
           .Extended_Return_Statement_Access);

      procedure Accept_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Accept_Statements
           .Accept_Statement_Access);

      procedure Requeue_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Requeue_Statements
           .Requeue_Statement_Access);

      procedure Delay_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Delay_Statements
           .Delay_Statement_Access);

      procedure Terminate_Alternative_Statement
        (Self    : in out Visitor;
         Element : not null Program.Elements.Terminate_Alternative_Statements
           .Terminate_Alternative_Statement_Access);

      procedure Select_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Select_Statements
           .Select_Statement_Access);

      procedure Abort_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Abort_Statements
           .Abort_Statement_Access);

      procedure Raise_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Raise_Statements
           .Raise_Statement_Access);

      procedure Code_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Code_Statements
           .Code_Statement_Access);

      procedure Elsif_Path
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Elsif_Paths.Elsif_Path_Access);

      procedure Case_Path
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Case_Paths.Case_Path_Access);

      procedure Select_Path
        (Self    : in out Visitor;
         Element : not null Program.Elements.Select_Paths.Select_Path_Access);

      procedure Case_Expression_Path
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Case_Expression_Paths
           .Case_Expression_Path_Access);

      procedure Elsif_Expression_Path
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Elsif_Expression_Paths
           .Elsif_Expression_Path_Access);

      procedure Use_Clause
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Use_Clauses.Use_Clause_Access);

      procedure With_Clause
        (Self    : in out Visitor;
         Element : not null Program.Elements.With_Clauses.With_Clause_Access);

      procedure Component_Clause
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Component_Clauses
           .Component_Clause_Access);

      procedure Derived_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Derived_Types
           .Derived_Type_Access);

      procedure Derived_Record_Extension
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Derived_Record_Extensions
           .Derived_Record_Extension_Access);

      procedure Enumeration_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Enumeration_Types
           .Enumeration_Type_Access);

      procedure Signed_Integer_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Signed_Integer_Types
           .Signed_Integer_Type_Access);

      procedure Modular_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Modular_Types
           .Modular_Type_Access);

      procedure Root_Type
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Root_Types.Root_Type_Access);

      procedure Floating_Point_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Floating_Point_Types
           .Floating_Point_Type_Access);

      procedure Ordinary_Fixed_Point_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Ordinary_Fixed_Point_Types
           .Ordinary_Fixed_Point_Type_Access);

      procedure Decimal_Fixed_Point_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Decimal_Fixed_Point_Types
           .Decimal_Fixed_Point_Type_Access);

      procedure Unconstrained_Array_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Unconstrained_Array_Types
           .Unconstrained_Array_Type_Access);

      procedure Constrained_Array_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Constrained_Array_Types
           .Constrained_Array_Type_Access);

      procedure Record_Type
        (Self    : in out Visitor;
         Element : not null Program.Elements.Record_Types.Record_Type_Access);

      procedure Interface_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Interface_Types
           .Interface_Type_Access);

      procedure Object_Access_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Object_Access_Types
           .Object_Access_Type_Access);

      procedure Procedure_Access_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Procedure_Access_Types
           .Procedure_Access_Type_Access);

      procedure Function_Access_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Function_Access_Types
           .Function_Access_Type_Access);

      procedure Formal_Private_Type_Definition
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Formal_Private_Type_Definitions
           .Formal_Private_Type_Definition_Access);

      procedure Formal_Derived_Type_Definition
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Formal_Derived_Type_Definitions
           .Formal_Derived_Type_Definition_Access);

      procedure Formal_Discrete_Type_Definition
        (Self    : in out Visitor;
         Element : not null Program.Elements.Formal_Discrete_Type_Definitions
           .Formal_Discrete_Type_Definition_Access);

      procedure Formal_Signed_Integer_Type_Definition
        (Self    : in out Visitor;
         Element :        not null Program.Elements
           .Formal_Signed_Integer_Type_Definitions
           .Formal_Signed_Integer_Type_Definition_Access);

      procedure Formal_Modular_Type_Definition
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Formal_Modular_Type_Definitions
           .Formal_Modular_Type_Definition_Access);

      procedure Formal_Floating_Point_Definition
        (Self    : in out Visitor;
         Element : not null Program.Elements.Formal_Floating_Point_Definitions
           .Formal_Floating_Point_Definition_Access);

      procedure Formal_Ordinary_Fixed_Point_Definition
        (Self    : in out Visitor;
         Element :        not null Program.Elements
           .Formal_Ordinary_Fixed_Point_Definitions
           .Formal_Ordinary_Fixed_Point_Definition_Access);

      procedure Formal_Decimal_Fixed_Point_Definition
        (Self    : in out Visitor;
         Element :        not null Program.Elements
           .Formal_Decimal_Fixed_Point_Definitions
           .Formal_Decimal_Fixed_Point_Definition_Access);

      procedure Formal_Unconstrained_Array_Type
        (Self    : in out Visitor;
         Element : not null Program.Elements.Formal_Unconstrained_Array_Types
           .Formal_Unconstrained_Array_Type_Access);

      procedure Formal_Constrained_Array_Type
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Formal_Constrained_Array_Types
           .Formal_Constrained_Array_Type_Access);

      procedure Formal_Object_Access_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Formal_Object_Access_Types
           .Formal_Object_Access_Type_Access);

      procedure Formal_Procedure_Access_Type
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Formal_Procedure_Access_Types
           .Formal_Procedure_Access_Type_Access);

      procedure Formal_Function_Access_Type
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Formal_Function_Access_Types
           .Formal_Function_Access_Type_Access);

      procedure Formal_Interface_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Formal_Interface_Types
           .Formal_Interface_Type_Access);

      procedure Range_Attribute_Reference
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Range_Attribute_References
           .Range_Attribute_Reference_Access);

      procedure Simple_Expression_Range
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Simple_Expression_Ranges
           .Simple_Expression_Range_Access);

      procedure Digits_Constraint
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Digits_Constraints
           .Digits_Constraint_Access);

      procedure Delta_Constraint
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Delta_Constraints
           .Delta_Constraint_Access);

      procedure Index_Constraint
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Index_Constraints
           .Index_Constraint_Access);

      procedure Discriminant_Constraint
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Discriminant_Constraints
           .Discriminant_Constraint_Access);

      procedure Attribute_Definition_Clause
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Attribute_Definition_Clauses
           .Attribute_Definition_Clause_Access);

      procedure Enumeration_Representation_Clause
        (Self    : in out Visitor;
         Element : not null Program.Elements.Enumeration_Representation_Clauses
           .Enumeration_Representation_Clause_Access);

      procedure Record_Representation_Clause
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Record_Representation_Clauses
           .Record_Representation_Clause_Access);

      procedure At_Clause
        (Self    : in out Visitor;
         Element :    not null Program.Elements.At_Clauses.At_Clause_Access);

      procedure Exception_Handler
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Exception_Handlers
           .Exception_Handler_Access);

   end Visitors;

   package body Visitors is

      procedure Print
        (Self    : in out Visitor'Class;
         Element : access Program.Elements.Element'Class;
         Text    : Wide_Wide_String);

      procedure Print
        (Self    : in out Visitor'Class;
         Element : access Program.Elements.Element'Class;
         Text    : Wide_Wide_String)
      is
         S : constant Wide_Wide_String :=
           "                                                               " &
           "                                                               " &
           "                                                               " &
           "                                                               ";
      begin
         Ada.Wide_Wide_Text_IO.Put_Line (S (1 .. Self.Level) & Text);
         Self.Level := Self.Level + 1;

         for Cursor in Element.Each_Child loop
            Cursor.Element.Visit (Self);
         end loop;

         Self.Level := Self.Level - 1;
      end Print;

      procedure Pragma_Element
        (Self    : in out Visitor;
         Element : not null Program.Elements.Pragmas.Pragma_Access)
      is
      begin
         Self.Print (Element, "Pragma_Element");
      end Pragma_Element;

      procedure Defining_Identifier
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Defining_Identifiers
           .Defining_Identifier_Access)
      is
      begin
         Self.Print (Element, "Defining_Identifier");
      end Defining_Identifier;

      procedure Defining_Character_Literal
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Defining_Character_Literals
           .Defining_Character_Literal_Access)
      is
      begin
         Self.Print (Element, "Defining_Character_Literal");
      end Defining_Character_Literal;

      procedure Defining_Operator_Symbol
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Defining_Operator_Symbols
           .Defining_Operator_Symbol_Access)
      is
      begin
         Self.Print (Element, "Defining_Operator_Symbol");
      end Defining_Operator_Symbol;

      procedure Defining_Expanded_Name
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Defining_Expanded_Names
           .Defining_Expanded_Name_Access)
      is
      begin
         Self.Print (Element, "Defining_Expanded_Name");
      end Defining_Expanded_Name;

      procedure Type_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Type_Declarations
           .Type_Declaration_Access)
      is
      begin
         Self.Print (Element, "Type_Declaration");
      end Type_Declaration;

      procedure Task_Type_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Task_Type_Declarations
           .Task_Type_Declaration_Access)
      is
      begin
         Self.Print (Element, "Task_Type_Declaration");
      end Task_Type_Declaration;

      procedure Protected_Type_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Protected_Type_Declarations
           .Protected_Type_Declaration_Access)
      is
      begin
         Self.Print (Element, "Protected_Type_Declaration");
      end Protected_Type_Declaration;

      procedure Subtype_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Subtype_Declarations
           .Subtype_Declaration_Access)
      is
      begin
         Self.Print (Element, "Subtype_Declaration");
      end Subtype_Declaration;

      procedure Object_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Object_Declarations
           .Object_Declaration_Access)
      is
      begin
         Self.Print (Element, "Object_Declaration");
      end Object_Declaration;

      procedure Single_Task_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Single_Task_Declarations
           .Single_Task_Declaration_Access)
      is
      begin
         Self.Print (Element, "Single_Task_Declaration");
      end Single_Task_Declaration;

      procedure Single_Protected_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Single_Protected_Declarations
           .Single_Protected_Declaration_Access)
      is
      begin
         Self.Print (Element, "Single_Protected_Declaration");
      end Single_Protected_Declaration;

      procedure Number_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Number_Declarations
           .Number_Declaration_Access)
      is
      begin
         Self.Print (Element, "Number_Declaration");
      end Number_Declaration;

      procedure Enumeration_Literal_Specification
        (Self    : in out Visitor;
         Element : not null Program.Elements.Enumeration_Literal_Specifications
           .Enumeration_Literal_Specification_Access)
      is
      begin
         Self.Print (Element, "Enumeration_Literal_Specification");
      end Enumeration_Literal_Specification;

      procedure Discriminant_Specification
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Discriminant_Specifications
           .Discriminant_Specification_Access)
      is
      begin
         Self.Print (Element, "Discriminant_Specification");
      end Discriminant_Specification;

      procedure Component_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Component_Declarations
           .Component_Declaration_Access)
      is
      begin
         Self.Print (Element, "Component_Declaration");
      end Component_Declaration;

      procedure Loop_Parameter_Specification
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Loop_Parameter_Specifications
           .Loop_Parameter_Specification_Access)
      is
      begin
         Self.Print (Element, "Loop_Parameter_Specification");
      end Loop_Parameter_Specification;

      procedure Generalized_Iterator_Specification
        (Self    : in out Visitor;
         Element :        not null Program.Elements
           .Generalized_Iterator_Specifications
           .Generalized_Iterator_Specification_Access)
      is
      begin
         Self.Print (Element, "Generalized_Iterator_Specification");
      end Generalized_Iterator_Specification;

      procedure Element_Iterator_Specification
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Element_Iterator_Specifications
           .Element_Iterator_Specification_Access)
      is
      begin
         Self.Print (Element, "Element_Iterator_Specification");
      end Element_Iterator_Specification;

      procedure Procedure_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Procedure_Declarations
           .Procedure_Declaration_Access)
      is
      begin
         Self.Print (Element, "Procedure_Declaration");
      end Procedure_Declaration;

      procedure Function_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Function_Declarations
           .Function_Declaration_Access)
      is
      begin
         Self.Print (Element, "Function_Declaration");
      end Function_Declaration;

      procedure Parameter_Specification
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Parameter_Specifications
           .Parameter_Specification_Access)
      is
      begin
         Self.Print (Element, "Parameter_Specification");
      end Parameter_Specification;

      procedure Procedure_Body_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Procedure_Body_Declarations
           .Procedure_Body_Declaration_Access)
      is
      begin
         Self.Print (Element, "Procedure_Body_Declaration");
      end Procedure_Body_Declaration;

      procedure Function_Body_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Function_Body_Declarations
           .Function_Body_Declaration_Access)
      is
      begin
         Self.Print (Element, "Function_Body_Declaration");
      end Function_Body_Declaration;

      procedure Return_Object_Specification
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Return_Object_Specifications
           .Return_Object_Specification_Access)
      is
      begin
         Self.Print (Element, "Return_Object_Specification");
      end Return_Object_Specification;

      procedure Package_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Package_Declarations
           .Package_Declaration_Access)
      is
      begin
         Self.Print (Element, "Package_Declaration");
      end Package_Declaration;

      procedure Package_Body_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Package_Body_Declarations
           .Package_Body_Declaration_Access)
      is
      begin
         Self.Print (Element, "Package_Body_Declaration");
      end Package_Body_Declaration;

      procedure Object_Renaming_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Object_Renaming_Declarations
           .Object_Renaming_Declaration_Access)
      is
      begin
         Self.Print (Element, "Object_Renaming_Declaration");
      end Object_Renaming_Declaration;

      procedure Exception_Renaming_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Exception_Renaming_Declarations
           .Exception_Renaming_Declaration_Access)
      is
      begin
         Self.Print (Element, "Exception_Renaming_Declaration");
      end Exception_Renaming_Declaration;

      procedure Procedure_Renaming_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Procedure_Renaming_Declarations
           .Procedure_Renaming_Declaration_Access)
      is
      begin
         Self.Print (Element, "Procedure_Renaming_Declaration");
      end Procedure_Renaming_Declaration;

      procedure Function_Renaming_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Function_Renaming_Declarations
           .Function_Renaming_Declaration_Access)
      is
      begin
         Self.Print (Element, "Function_Renaming_Declaration");
      end Function_Renaming_Declaration;

      procedure Package_Renaming_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Package_Renaming_Declarations
           .Package_Renaming_Declaration_Access)
      is
      begin
         Self.Print (Element, "Package_Renaming_Declaration");
      end Package_Renaming_Declaration;

      procedure Generic_Package_Renaming_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements
           .Generic_Package_Renaming_Declarations
           .Generic_Package_Renaming_Declaration_Access)
      is
      begin
         Self.Print (Element, "Generic_Package_Renaming_Declaration");
      end Generic_Package_Renaming_Declaration;

      procedure Generic_Procedure_Renaming_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements
           .Generic_Procedure_Renaming_Declarations
           .Generic_Procedure_Renaming_Declaration_Access)
      is
      begin
         Self.Print (Element, "Generic_Procedure_Renaming_Declaration");
      end Generic_Procedure_Renaming_Declaration;

      procedure Generic_Function_Renaming_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements
           .Generic_Function_Renaming_Declarations
           .Generic_Function_Renaming_Declaration_Access)
      is
      begin
         Self.Print (Element, "Generic_Function_Renaming_Declaration");
      end Generic_Function_Renaming_Declaration;

      procedure Task_Body_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Task_Body_Declarations
           .Task_Body_Declaration_Access)
      is
      begin
         Self.Print (Element, "Task_Body_Declaration");
      end Task_Body_Declaration;

      procedure Protected_Body_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Protected_Body_Declarations
           .Protected_Body_Declaration_Access)
      is
      begin
         Self.Print (Element, "Protected_Body_Declaration");
      end Protected_Body_Declaration;

      procedure Entry_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Entry_Declarations
           .Entry_Declaration_Access)
      is
      begin
         Self.Print (Element, "Entry_Declaration");
      end Entry_Declaration;

      procedure Entry_Body_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Entry_Body_Declarations
           .Entry_Body_Declaration_Access)
      is
      begin
         Self.Print (Element, "Entry_Body_Declaration");
      end Entry_Body_Declaration;

      procedure Entry_Index_Specification
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Entry_Index_Specifications
           .Entry_Index_Specification_Access)
      is
      begin
         Self.Print (Element, "Entry_Index_Specification");
      end Entry_Index_Specification;

      procedure Procedure_Body_Stub
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Procedure_Body_Stubs
           .Procedure_Body_Stub_Access)
      is
      begin
         Self.Print (Element, "Procedure_Body_Stub");
      end Procedure_Body_Stub;

      procedure Function_Body_Stub
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Function_Body_Stubs
           .Function_Body_Stub_Access)
      is
      begin
         Self.Print (Element, "Function_Body_Stub");
      end Function_Body_Stub;

      procedure Package_Body_Stub
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Package_Body_Stubs
           .Package_Body_Stub_Access)
      is
      begin
         Self.Print (Element, "Package_Body_Stub");
      end Package_Body_Stub;

      procedure Task_Body_Stub
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Task_Body_Stubs
           .Task_Body_Stub_Access)
      is
      begin
         Self.Print (Element, "Task_Body_Stub");
      end Task_Body_Stub;

      procedure Protected_Body_Stub
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Protected_Body_Stubs
           .Protected_Body_Stub_Access)
      is
      begin
         Self.Print (Element, "Protected_Body_Stub");
      end Protected_Body_Stub;

      procedure Exception_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Exception_Declarations
           .Exception_Declaration_Access)
      is
      begin
         Self.Print (Element, "Exception_Declaration");
      end Exception_Declaration;

      procedure Choice_Parameter_Specification
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Choice_Parameter_Specifications
           .Choice_Parameter_Specification_Access)
      is
      begin
         Self.Print (Element, "Choice_Parameter_Specification");
      end Choice_Parameter_Specification;

      procedure Generic_Package_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Generic_Package_Declarations
           .Generic_Package_Declaration_Access)
      is
      begin
         Self.Print (Element, "Generic_Package_Declaration");
      end Generic_Package_Declaration;

      procedure Generic_Procedure_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Generic_Procedure_Declarations
           .Generic_Procedure_Declaration_Access)
      is
      begin
         Self.Print (Element, "Generic_Procedure_Declaration");
      end Generic_Procedure_Declaration;

      procedure Generic_Function_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Generic_Function_Declarations
           .Generic_Function_Declaration_Access)
      is
      begin
         Self.Print (Element, "Generic_Function_Declaration");
      end Generic_Function_Declaration;

      procedure Package_Instantiation
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Package_Instantiations
           .Package_Instantiation_Access)
      is
      begin
         Self.Print (Element, "Package_Instantiation");
      end Package_Instantiation;

      procedure Procedure_Instantiation
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Procedure_Instantiations
           .Procedure_Instantiation_Access)
      is
      begin
         Self.Print (Element, "Procedure_Instantiation");
      end Procedure_Instantiation;

      procedure Function_Instantiation
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Function_Instantiations
           .Function_Instantiation_Access)
      is
      begin
         Self.Print (Element, "Function_Instantiation");
      end Function_Instantiation;

      procedure Formal_Object_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Formal_Object_Declarations
           .Formal_Object_Declaration_Access)
      is
      begin
         Self.Print (Element, "Formal_Object_Declaration");
      end Formal_Object_Declaration;

      procedure Formal_Type_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Formal_Type_Declarations
           .Formal_Type_Declaration_Access)
      is
      begin
         Self.Print (Element, "Formal_Type_Declaration");
      end Formal_Type_Declaration;

      procedure Formal_Procedure_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Formal_Procedure_Declarations
           .Formal_Procedure_Declaration_Access)
      is
      begin
         Self.Print (Element, "Formal_Procedure_Declaration");
      end Formal_Procedure_Declaration;

      procedure Formal_Function_Declaration
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Formal_Function_Declarations
           .Formal_Function_Declaration_Access)
      is
      begin
         Self.Print (Element, "Formal_Function_Declaration");
      end Formal_Function_Declaration;

      procedure Formal_Package_Declaration
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Formal_Package_Declarations
           .Formal_Package_Declaration_Access)
      is
      begin
         Self.Print (Element, "Formal_Package_Declaration");
      end Formal_Package_Declaration;

      procedure Subtype_Indication
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Subtype_Indications
           .Subtype_Indication_Access)
      is
      begin
         Self.Print (Element, "Subtype_Indication");
      end Subtype_Indication;

      procedure Component_Definition
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Component_Definitions
           .Component_Definition_Access)
      is
      begin
         Self.Print (Element, "Component_Definition");
      end Component_Definition;

      procedure Discrete_Subtype_Indication
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Discrete_Subtype_Indications
           .Discrete_Subtype_Indication_Access)
      is
      begin
         Self.Print (Element, "Discrete_Subtype_Indication");
      end Discrete_Subtype_Indication;

      procedure Discrete_Range_Attribute_Reference
        (Self    : in out Visitor;
         Element :        not null Program.Elements
           .Discrete_Range_Attribute_References
           .Discrete_Range_Attribute_Reference_Access)
      is
      begin
         Self.Print (Element, "Discrete_Range_Attribute_Reference");
      end Discrete_Range_Attribute_Reference;

      procedure Discrete_Simple_Expression_Range
        (Self    : in out Visitor;
         Element : not null Program.Elements.Discrete_Simple_Expression_Ranges
           .Discrete_Simple_Expression_Range_Access)
      is
      begin
         Self.Print (Element, "Discrete_Simple_Expression_Range");
      end Discrete_Simple_Expression_Range;

      procedure Unknown_Discriminant_Part
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Unknown_Discriminant_Parts
           .Unknown_Discriminant_Part_Access)
      is
      begin
         Self.Print (Element, "Unknown_Discriminant_Part");
      end Unknown_Discriminant_Part;

      procedure Known_Discriminant_Part
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Known_Discriminant_Parts
           .Known_Discriminant_Part_Access)
      is
      begin
         Self.Print (Element, "Known_Discriminant_Part");
      end Known_Discriminant_Part;

      procedure Record_Definition
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Record_Definitions
           .Record_Definition_Access)
      is
      begin
         Self.Print (Element, "Record_Definition");
      end Record_Definition;

      procedure Null_Component
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Null_Components
           .Null_Component_Access)
      is
      begin
         Self.Print (Element, "Null_Component");
      end Null_Component;

      procedure Variant_Part
        (Self    : in out Visitor;
         Element : not null Program.Elements.Variant_Parts.Variant_Part_Access)
      is
      begin
         Self.Print (Element, "Variant_Part");
      end Variant_Part;

      procedure Variant
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Variants.Variant_Access)
      is
      begin
         Self.Print (Element, "Variant");
      end Variant;

      procedure Others_Choice
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Others_Choices
           .Others_Choice_Access)
      is
      begin
         Self.Print (Element, "Others_Choice");
      end Others_Choice;

      procedure Anonymous_Access_To_Object
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Anonymous_Access_To_Objects
           .Anonymous_Access_To_Object_Access)
      is
      begin
         Self.Print (Element, "Anonymous_Access_To_Object");
      end Anonymous_Access_To_Object;

      procedure Anonymous_Access_To_Procedure
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Anonymous_Access_To_Procedures
           .Anonymous_Access_To_Procedure_Access)
      is
      begin
         Self.Print (Element, "Anonymous_Access_To_Procedure");
      end Anonymous_Access_To_Procedure;

      procedure Anonymous_Access_To_Function
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Anonymous_Access_To_Functions
           .Anonymous_Access_To_Function_Access)
      is
      begin
         Self.Print (Element, "Anonymous_Access_To_Function");
      end Anonymous_Access_To_Function;

      procedure Private_Type_Definition
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Private_Type_Definitions
           .Private_Type_Definition_Access)
      is
      begin
         Self.Print (Element, "Private_Type_Definition");
      end Private_Type_Definition;

      procedure Private_Extension_Definition
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Private_Extension_Definitions
           .Private_Extension_Definition_Access)
      is
      begin
         Self.Print (Element, "Private_Extension_Definition");
      end Private_Extension_Definition;

      procedure Incomplete_Type_Definition
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Incomplete_Type_Definitions
           .Incomplete_Type_Definition_Access)
      is
      begin
         Self.Print (Element, "Incomplete_Type_Definition");
      end Incomplete_Type_Definition;

      procedure Task_Definition
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Task_Definitions
           .Task_Definition_Access)
      is
      begin
         Self.Print (Element, "Task_Definition");
      end Task_Definition;

      procedure Protected_Definition
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Protected_Definitions
           .Protected_Definition_Access)
      is
      begin
         Self.Print (Element, "Protected_Definition");
      end Protected_Definition;

      procedure Aspect_Specification
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Aspect_Specifications
           .Aspect_Specification_Access)
      is
      begin
         Self.Print (Element, "Aspect_Specification");
      end Aspect_Specification;

      procedure Real_Range_Specification
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Real_Range_Specifications
           .Real_Range_Specification_Access)
      is
      begin
         Self.Print (Element, "Real_Range_Specification");
      end Real_Range_Specification;

      procedure Numeric_Literal
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Numeric_Literals
           .Numeric_Literal_Access)
      is
      begin
         Self.Print (Element, "Numeric_Literal");
      end Numeric_Literal;

      procedure String_Literal
        (Self    : in out Visitor;
         Element :        not null Program.Elements.String_Literals
           .String_Literal_Access)
      is
      begin
         Self.Print (Element, "String_Literal");
      end String_Literal;

      procedure Identifier
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Identifiers.Identifier_Access)
      is
      begin
         Self.Print (Element, "Identifier");
      end Identifier;

      procedure Operator_Symbol
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Operator_Symbols
           .Operator_Symbol_Access)
      is
      begin
         Self.Print (Element, "Operator_Symbol");
      end Operator_Symbol;

      procedure Character_Literal
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Character_Literals
           .Character_Literal_Access)
      is
      begin
         Self.Print (Element, "Character_Literal");
      end Character_Literal;

      procedure Explicit_Dereference
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Explicit_Dereferences
           .Explicit_Dereference_Access)
      is
      begin
         Self.Print (Element, "Explicit_Dereference");
      end Explicit_Dereference;

      procedure Infix_Operator
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Infix_Operators
           .Infix_Operator_Access)
      is
      begin
         Self.Print (Element, "Infix_Operator");
      end Infix_Operator;

      procedure Function_Call
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Function_Calls
           .Function_Call_Access)
      is
      begin
         Self.Print (Element, "Function_Call");
      end Function_Call;

      procedure Indexed_Component
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Indexed_Components
           .Indexed_Component_Access)
      is
      begin
         Self.Print (Element, "Indexed_Component");
      end Indexed_Component;

      procedure Slice
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Slices.Slice_Access)
      is
      begin
         Self.Print (Element, "Slice");
      end Slice;

      procedure Selected_Component
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Selected_Components
           .Selected_Component_Access)
      is
      begin
         Self.Print (Element, "Selected_Component");
      end Selected_Component;

      procedure Attribute_Reference
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Attribute_References
           .Attribute_Reference_Access)
      is
      begin
         Self.Print (Element, "Attribute_Reference");
      end Attribute_Reference;

      procedure Record_Aggregate
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Record_Aggregates
           .Record_Aggregate_Access)
      is
      begin
         Self.Print (Element, "Record_Aggregate");
      end Record_Aggregate;

      procedure Extension_Aggregate
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Extension_Aggregates
           .Extension_Aggregate_Access)
      is
      begin
         Self.Print (Element, "Extension_Aggregate");
      end Extension_Aggregate;

      procedure Array_Aggregate
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Array_Aggregates
           .Array_Aggregate_Access)
      is
      begin
         Self.Print (Element, "Array_Aggregate");
      end Array_Aggregate;

      procedure Short_Circuit_Operation
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Short_Circuit_Operations
           .Short_Circuit_Operation_Access)
      is
      begin
         Self.Print (Element, "Short_Circuit_Operation");
      end Short_Circuit_Operation;

      procedure Membership_Test
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Membership_Tests
           .Membership_Test_Access)
      is
      begin
         Self.Print (Element, "Membership_Test");
      end Membership_Test;

      procedure Null_Literal
        (Self    : in out Visitor;
         Element : not null Program.Elements.Null_Literals.Null_Literal_Access)
      is
      begin
         Self.Print (Element, "Null_Literal");
      end Null_Literal;

      procedure Parenthesized_Expression
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Parenthesized_Expressions
           .Parenthesized_Expression_Access)
      is
      begin
         Self.Print (Element, "Parenthesized_Expression");
      end Parenthesized_Expression;

      procedure Raise_Expression
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Raise_Expressions
           .Raise_Expression_Access)
      is
      begin
         Self.Print (Element, "Raise_Expression");
      end Raise_Expression;

      procedure Type_Conversion
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Type_Conversions
           .Type_Conversion_Access)
      is
      begin
         Self.Print (Element, "Type_Conversion");
      end Type_Conversion;

      procedure Qualified_Expression
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Qualified_Expressions
           .Qualified_Expression_Access)
      is
      begin
         Self.Print (Element, "Qualified_Expression");
      end Qualified_Expression;

      procedure Allocator
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Allocators.Allocator_Access)
      is
      begin
         Self.Print (Element, "Allocator");
      end Allocator;

      procedure Case_Expression
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Case_Expressions
           .Case_Expression_Access)
      is
      begin
         Self.Print (Element, "Case_Expression");
      end Case_Expression;

      procedure If_Expression
        (Self    : in out Visitor;
         Element :        not null Program.Elements.If_Expressions
           .If_Expression_Access)
      is
      begin
         Self.Print (Element, "If_Expression");
      end If_Expression;

      procedure Quantified_Expression
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Quantified_Expressions
           .Quantified_Expression_Access)
      is
      begin
         Self.Print (Element, "Quantified_Expression");
      end Quantified_Expression;

      procedure Discriminant_Association
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Discriminant_Associations
           .Discriminant_Association_Access)
      is
      begin
         Self.Print (Element, "Discriminant_Association");
      end Discriminant_Association;

      procedure Record_Component_Association
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Record_Component_Associations
           .Record_Component_Association_Access)
      is
      begin
         Self.Print (Element, "Record_Component_Association");
      end Record_Component_Association;

      procedure Array_Component_Association
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Array_Component_Associations
           .Array_Component_Association_Access)
      is
      begin
         Self.Print (Element, "Array_Component_Association");
      end Array_Component_Association;

      procedure Parameter_Association
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Parameter_Associations
           .Parameter_Association_Access)
      is
      begin
         Self.Print (Element, "Parameter_Association");
      end Parameter_Association;

      procedure Formal_Package_Association
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Formal_Package_Associations
           .Formal_Package_Association_Access)
      is
      begin
         Self.Print (Element, "Formal_Package_Association");
      end Formal_Package_Association;

      procedure Null_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Null_Statements
           .Null_Statement_Access)
      is
      begin
         Self.Print (Element, "Null_Statement");
      end Null_Statement;

      procedure Assignment_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Assignment_Statements
           .Assignment_Statement_Access)
      is
      begin
         Self.Print (Element, "Assignment_Statement");
      end Assignment_Statement;

      procedure If_Statement
        (Self    : in out Visitor;
         Element : not null Program.Elements.If_Statements.If_Statement_Access)
      is
      begin
         Self.Print (Element, "If_Statement");
      end If_Statement;

      procedure Case_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Case_Statements
           .Case_Statement_Access)
      is
      begin
         Self.Print (Element, "Case_Statement");
      end Case_Statement;

      procedure Loop_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Loop_Statements
           .Loop_Statement_Access)
      is
      begin
         Self.Print (Element, "Loop_Statement");
      end Loop_Statement;

      procedure While_Loop_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.While_Loop_Statements
           .While_Loop_Statement_Access)
      is
      begin
         Self.Print (Element, "While_Loop_Statement");
      end While_Loop_Statement;

      procedure For_Loop_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.For_Loop_Statements
           .For_Loop_Statement_Access)
      is
      begin
         Self.Print (Element, "For_Loop_Statement");
      end For_Loop_Statement;

      procedure Block_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Block_Statements
           .Block_Statement_Access)
      is
      begin
         Self.Print (Element, "Block_Statement");
      end Block_Statement;

      procedure Exit_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Exit_Statements
           .Exit_Statement_Access)
      is
      begin
         Self.Print (Element, "Exit_Statement");
      end Exit_Statement;

      procedure Goto_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Goto_Statements
           .Goto_Statement_Access)
      is
      begin
         Self.Print (Element, "Goto_Statement");
      end Goto_Statement;

      procedure Call_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Call_Statements
           .Call_Statement_Access)
      is
      begin
         Self.Print (Element, "Call_Statement");
      end Call_Statement;

      procedure Simple_Return_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Simple_Return_Statements
           .Simple_Return_Statement_Access)
      is
      begin
         Self.Print (Element, "Simple_Return_Statement");
      end Simple_Return_Statement;

      procedure Extended_Return_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Extended_Return_Statements
           .Extended_Return_Statement_Access)
      is
      begin
         Self.Print (Element, "Extended_Return_Statement");
      end Extended_Return_Statement;

      procedure Accept_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Accept_Statements
           .Accept_Statement_Access)
      is
      begin
         Self.Print (Element, "Accept_Statement");
      end Accept_Statement;

      procedure Requeue_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Requeue_Statements
           .Requeue_Statement_Access)
      is
      begin
         Self.Print (Element, "Requeue_Statement");
      end Requeue_Statement;

      procedure Delay_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Delay_Statements
           .Delay_Statement_Access)
      is
      begin
         Self.Print (Element, "Delay_Statement");
      end Delay_Statement;

      procedure Terminate_Alternative_Statement
        (Self    : in out Visitor;
         Element : not null Program.Elements.Terminate_Alternative_Statements
           .Terminate_Alternative_Statement_Access)
      is
      begin
         Self.Print (Element, "Terminate_Alternative_Statement");
      end Terminate_Alternative_Statement;

      procedure Select_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Select_Statements
           .Select_Statement_Access)
      is
      begin
         Self.Print (Element, "Select_Statement");
      end Select_Statement;

      procedure Abort_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Abort_Statements
           .Abort_Statement_Access)
      is
      begin
         Self.Print (Element, "Abort_Statement");
      end Abort_Statement;

      procedure Raise_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Raise_Statements
           .Raise_Statement_Access)
      is
      begin
         Self.Print (Element, "Raise_Statement");
      end Raise_Statement;

      procedure Code_Statement
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Code_Statements
           .Code_Statement_Access)
      is
      begin
         Self.Print (Element, "Code_Statement");
      end Code_Statement;

      procedure Elsif_Path
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Elsif_Paths.Elsif_Path_Access)
      is
      begin
         Self.Print (Element, "Elsif_Path");
      end Elsif_Path;

      procedure Case_Path
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Case_Paths.Case_Path_Access)
      is
      begin
         Self.Print (Element, "Case_Path");
      end Case_Path;

      procedure Select_Path
        (Self    : in out Visitor;
         Element : not null Program.Elements.Select_Paths.Select_Path_Access)
      is
      begin
         Self.Print (Element, "Select_Path");
      end Select_Path;

      procedure Case_Expression_Path
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Case_Expression_Paths
           .Case_Expression_Path_Access)
      is
      begin
         Self.Print (Element, "Case_Expression_Path");
      end Case_Expression_Path;

      procedure Elsif_Expression_Path
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Elsif_Expression_Paths
           .Elsif_Expression_Path_Access)
      is
      begin
         Self.Print (Element, "Elsif_Expression_Path");
      end Elsif_Expression_Path;

      procedure Use_Clause
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Use_Clauses.Use_Clause_Access)
      is
      begin
         Self.Print (Element, "Use_Clause");
      end Use_Clause;

      procedure With_Clause
        (Self    : in out Visitor;
         Element : not null Program.Elements.With_Clauses.With_Clause_Access)
      is
      begin
         Self.Print (Element, "With_Clause");
      end With_Clause;

      procedure Component_Clause
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Component_Clauses
           .Component_Clause_Access)
      is
      begin
         Self.Print (Element, "Component_Clause");
      end Component_Clause;

      procedure Derived_Type
        (Self    : in out Visitor;
         Element : not null Program.Elements.Derived_Types.Derived_Type_Access)
      is
      begin
         Self.Print (Element, "Derived_Type");
      end Derived_Type;

      procedure Derived_Record_Extension
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Derived_Record_Extensions
           .Derived_Record_Extension_Access)
      is
      begin
         Self.Print (Element, "Derived_Record_Extension");
      end Derived_Record_Extension;

      procedure Enumeration_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Enumeration_Types
           .Enumeration_Type_Access)
      is
      begin
         Self.Print (Element, "Enumeration_Type");
      end Enumeration_Type;

      procedure Signed_Integer_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Signed_Integer_Types
           .Signed_Integer_Type_Access)
      is
      begin
         Self.Print (Element, "Signed_Integer_Type");
      end Signed_Integer_Type;

      procedure Modular_Type
        (Self    : in out Visitor;
         Element : not null Program.Elements.Modular_Types.Modular_Type_Access)
      is
      begin
         Self.Print (Element, "Modular_Type");
      end Modular_Type;

      procedure Root_Type
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Root_Types.Root_Type_Access)
      is
      begin
         Self.Print (Element, "Root_Type");
      end Root_Type;

      procedure Floating_Point_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Floating_Point_Types
           .Floating_Point_Type_Access)
      is
      begin
         Self.Print (Element, "Floating_Point_Type");
      end Floating_Point_Type;

      procedure Ordinary_Fixed_Point_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Ordinary_Fixed_Point_Types
           .Ordinary_Fixed_Point_Type_Access)
      is
      begin
         Self.Print (Element, "Ordinary_Fixed_Point_Type");
      end Ordinary_Fixed_Point_Type;

      procedure Decimal_Fixed_Point_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Decimal_Fixed_Point_Types
           .Decimal_Fixed_Point_Type_Access)
      is
      begin
         Self.Print (Element, "Decimal_Fixed_Point_Type");
      end Decimal_Fixed_Point_Type;

      procedure Unconstrained_Array_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Unconstrained_Array_Types
           .Unconstrained_Array_Type_Access)
      is
      begin
         Self.Print (Element, "Unconstrained_Array_Type");
      end Unconstrained_Array_Type;

      procedure Constrained_Array_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Constrained_Array_Types
           .Constrained_Array_Type_Access)
      is
      begin
         Self.Print (Element, "Constrained_Array_Type");
      end Constrained_Array_Type;

      procedure Record_Type
        (Self    : in out Visitor;
         Element : not null Program.Elements.Record_Types.Record_Type_Access)
      is
      begin
         Self.Print (Element, "Record_Type");
      end Record_Type;

      procedure Interface_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Interface_Types
           .Interface_Type_Access)
      is
      begin
         Self.Print (Element, "Interface_Type");
      end Interface_Type;

      procedure Object_Access_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Object_Access_Types
           .Object_Access_Type_Access)
      is
      begin
         Self.Print (Element, "Object_Access_Type");
      end Object_Access_Type;

      procedure Procedure_Access_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Procedure_Access_Types
           .Procedure_Access_Type_Access)
      is
      begin
         Self.Print (Element, "Procedure_Access_Type");
      end Procedure_Access_Type;

      procedure Function_Access_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Function_Access_Types
           .Function_Access_Type_Access)
      is
      begin
         Self.Print (Element, "Function_Access_Type");
      end Function_Access_Type;

      procedure Formal_Private_Type_Definition
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Formal_Private_Type_Definitions
           .Formal_Private_Type_Definition_Access)
      is
      begin
         Self.Print (Element, "Formal_Private_Type_Definition");
      end Formal_Private_Type_Definition;

      procedure Formal_Derived_Type_Definition
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Formal_Derived_Type_Definitions
           .Formal_Derived_Type_Definition_Access)
      is
      begin
         Self.Print (Element, "Formal_Derived_Type_Definition");
      end Formal_Derived_Type_Definition;

      procedure Formal_Discrete_Type_Definition
        (Self    : in out Visitor;
         Element : not null Program.Elements.Formal_Discrete_Type_Definitions
           .Formal_Discrete_Type_Definition_Access)
      is
      begin
         Self.Print (Element, "Formal_Discrete_Type_Definition");
      end Formal_Discrete_Type_Definition;

      procedure Formal_Signed_Integer_Type_Definition
        (Self    : in out Visitor;
         Element :        not null Program.Elements
           .Formal_Signed_Integer_Type_Definitions
           .Formal_Signed_Integer_Type_Definition_Access)
      is
      begin
         Self.Print (Element, "Formal_Signed_Integer_Type_Definition");
      end Formal_Signed_Integer_Type_Definition;

      procedure Formal_Modular_Type_Definition
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Formal_Modular_Type_Definitions
           .Formal_Modular_Type_Definition_Access)
      is
      begin
         Self.Print (Element, "Formal_Modular_Type_Definition");
      end Formal_Modular_Type_Definition;

      procedure Formal_Floating_Point_Definition
        (Self    : in out Visitor;
         Element : not null Program.Elements.Formal_Floating_Point_Definitions
           .Formal_Floating_Point_Definition_Access)
      is
      begin
         Self.Print (Element, "Formal_Floating_Point_Definition");
      end Formal_Floating_Point_Definition;

      procedure Formal_Ordinary_Fixed_Point_Definition
        (Self    : in out Visitor;
         Element :        not null Program.Elements
           .Formal_Ordinary_Fixed_Point_Definitions
           .Formal_Ordinary_Fixed_Point_Definition_Access)
      is
      begin
         Self.Print (Element, "Formal_Ordinary_Fixed_Point_Definition");
      end Formal_Ordinary_Fixed_Point_Definition;

      procedure Formal_Decimal_Fixed_Point_Definition
        (Self    : in out Visitor;
         Element :        not null Program.Elements
           .Formal_Decimal_Fixed_Point_Definitions
           .Formal_Decimal_Fixed_Point_Definition_Access)
      is
      begin
         Self.Print (Element, "Formal_Decimal_Fixed_Point_Definition");
      end Formal_Decimal_Fixed_Point_Definition;

      procedure Formal_Unconstrained_Array_Type
        (Self    : in out Visitor;
         Element : not null Program.Elements.Formal_Unconstrained_Array_Types
           .Formal_Unconstrained_Array_Type_Access)
      is
      begin
         Self.Print (Element, "Formal_Unconstrained_Array_Type");
      end Formal_Unconstrained_Array_Type;

      procedure Formal_Constrained_Array_Type
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Formal_Constrained_Array_Types
           .Formal_Constrained_Array_Type_Access)
      is
      begin
         Self.Print (Element, "Formal_Constrained_Array_Type");
      end Formal_Constrained_Array_Type;

      procedure Formal_Object_Access_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Formal_Object_Access_Types
           .Formal_Object_Access_Type_Access)
      is
      begin
         Self.Print (Element, "Formal_Object_Access_Type");
      end Formal_Object_Access_Type;

      procedure Formal_Procedure_Access_Type
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Formal_Procedure_Access_Types
           .Formal_Procedure_Access_Type_Access)
      is
      begin
         Self.Print (Element, "Formal_Procedure_Access_Type");
      end Formal_Procedure_Access_Type;

      procedure Formal_Function_Access_Type
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Formal_Function_Access_Types
           .Formal_Function_Access_Type_Access)
      is
      begin
         Self.Print (Element, "Formal_Function_Access_Type");
      end Formal_Function_Access_Type;

      procedure Formal_Interface_Type
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Formal_Interface_Types
           .Formal_Interface_Type_Access)
      is
      begin
         Self.Print (Element, "Formal_Interface_Type");
      end Formal_Interface_Type;

      procedure Range_Attribute_Reference
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Range_Attribute_References
           .Range_Attribute_Reference_Access)
      is
      begin
         Self.Print (Element, "Range_Attribute_Reference");
      end Range_Attribute_Reference;

      procedure Simple_Expression_Range
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Simple_Expression_Ranges
           .Simple_Expression_Range_Access)
      is
      begin
         Self.Print (Element, "Simple_Expression_Range");
      end Simple_Expression_Range;

      procedure Digits_Constraint
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Digits_Constraints
           .Digits_Constraint_Access)
      is
      begin
         Self.Print (Element, "Digits_Constraint");
      end Digits_Constraint;

      procedure Delta_Constraint
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Delta_Constraints
           .Delta_Constraint_Access)
      is
      begin
         Self.Print (Element, "Delta_Constraint");
      end Delta_Constraint;

      procedure Index_Constraint
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Index_Constraints
           .Index_Constraint_Access)
      is
      begin
         Self.Print (Element, "Index_Constraint");
      end Index_Constraint;

      procedure Discriminant_Constraint
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Discriminant_Constraints
           .Discriminant_Constraint_Access)
      is
      begin
         Self.Print (Element, "Discriminant_Constraint");
      end Discriminant_Constraint;

      procedure Attribute_Definition_Clause
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Attribute_Definition_Clauses
           .Attribute_Definition_Clause_Access)
      is
      begin
         Self.Print (Element, "Attribute_Definition_Clause");
      end Attribute_Definition_Clause;

      procedure Enumeration_Representation_Clause
        (Self    : in out Visitor;
         Element : not null Program.Elements.Enumeration_Representation_Clauses
           .Enumeration_Representation_Clause_Access)
      is
      begin
         Self.Print (Element, "Enumeration_Representation_Clause");
      end Enumeration_Representation_Clause;

      procedure Record_Representation_Clause
        (Self    : in out Visitor;
         Element :    not null Program.Elements.Record_Representation_Clauses
           .Record_Representation_Clause_Access)
      is
      begin
         Self.Print (Element, "Record_Representation_Clause");
      end Record_Representation_Clause;

      procedure At_Clause
        (Self    : in out Visitor;
         Element :    not null Program.Elements.At_Clauses.At_Clause_Access)
      is
      begin
         Self.Print (Element, "At_Clause");
      end At_Clause;

      procedure Exception_Handler
        (Self    : in out Visitor;
         Element :        not null Program.Elements.Exception_Handlers
           .Exception_Handler_Access)
      is
      begin
         Self.Print (Element, "Exception_Handler");
      end Exception_Handler;

   end Visitors;

   -----------
   -- Print --
   -----------

   procedure Print (Element : Program.Elements.Element_Access) is
      V : Visitors.Visitor;
   begin
      Element.Visit (V);
   end Print;

end Dump_Elements;
