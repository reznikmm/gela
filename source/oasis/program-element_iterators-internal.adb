--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Pragmas;
with Program.Elements.Defining_Names;
with Program.Elements.Defining_Identifiers;
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
with Program.Elements.Constraints;
with Program.Elements.Component_Definitions;
with Program.Elements.Discrete_Ranges;
with Program.Elements.Discrete_Subtype_Indications;
with Program.Elements.Discrete_Range_Attribute_References;
with Program.Elements.Discrete_Simple_Expression_Ranges;
with Program.Elements.Known_Discriminant_Parts;
with Program.Elements.Record_Definitions;
with Program.Elements.Variant_Parts;
with Program.Elements.Variants;
with Program.Elements.Anonymous_Access_To_Objects;
with Program.Elements.Anonymous_Access_To_Procedures;
with Program.Elements.Anonymous_Access_To_Functions;
with Program.Elements.Private_Extension_Definitions;
with Program.Elements.Task_Definitions;
with Program.Elements.Protected_Definitions;
with Program.Elements.Formal_Type_Definitions;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Real_Range_Specifications;
with Program.Elements.Expressions;
with Program.Elements.Identifiers;
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
with Program.Elements.Formal_Derived_Type_Definitions;
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
with Program.Element_Visitors;

separate (Program.Element_Iterators)
package body Internal is

   type Visitor is
     new Program.Element_Visitors.Element_Visitor
     with record
        Result : access constant Getter_Array;
     end record;

   overriding procedure Pragma_Element
    (Self    : in out Visitor;
     Element : not null Program.Elements.Pragmas.Pragma_Access);

   overriding procedure Defining_Expanded_Name
    (Self    : in out Visitor;
     Element : not null Program.Elements.Defining_Expanded_Names
         .Defining_Expanded_Name_Access);

   overriding procedure Type_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Type_Declarations
         .Type_Declaration_Access);

   overriding procedure Task_Type_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Task_Type_Declarations
         .Task_Type_Declaration_Access);

   overriding procedure Protected_Type_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Protected_Type_Declarations
         .Protected_Type_Declaration_Access);

   overriding procedure Subtype_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Subtype_Declarations
         .Subtype_Declaration_Access);

   overriding procedure Object_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Object_Declarations
         .Object_Declaration_Access);

   overriding procedure Single_Task_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Single_Task_Declarations
         .Single_Task_Declaration_Access);

   overriding procedure Single_Protected_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Single_Protected_Declarations
         .Single_Protected_Declaration_Access);

   overriding procedure Number_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Number_Declarations
         .Number_Declaration_Access);

   overriding procedure Enumeration_Literal_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Enumeration_Literal_Specifications
         .Enumeration_Literal_Specification_Access);

   overriding procedure Discriminant_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Discriminant_Specifications
         .Discriminant_Specification_Access);

   overriding procedure Component_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Component_Declarations
         .Component_Declaration_Access);

   overriding procedure Loop_Parameter_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Loop_Parameter_Specifications
         .Loop_Parameter_Specification_Access);

   overriding procedure Generalized_Iterator_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Generalized_Iterator_Specifications
         .Generalized_Iterator_Specification_Access);

   overriding procedure Element_Iterator_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Element_Iterator_Specifications
         .Element_Iterator_Specification_Access);

   overriding procedure Procedure_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Procedure_Declarations
         .Procedure_Declaration_Access);

   overriding procedure Function_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Function_Declarations
         .Function_Declaration_Access);

   overriding procedure Parameter_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Access);

   overriding procedure Procedure_Body_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Procedure_Body_Declarations
         .Procedure_Body_Declaration_Access);

   overriding procedure Function_Body_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Function_Body_Declarations
         .Function_Body_Declaration_Access);

   overriding procedure Return_Object_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Return_Object_Specifications
         .Return_Object_Specification_Access);

   overriding procedure Package_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Package_Declarations
         .Package_Declaration_Access);

   overriding procedure Package_Body_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Package_Body_Declarations
         .Package_Body_Declaration_Access);

   overriding procedure Object_Renaming_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Object_Renaming_Declarations
         .Object_Renaming_Declaration_Access);

   overriding procedure Exception_Renaming_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Exception_Renaming_Declarations
         .Exception_Renaming_Declaration_Access);

   overriding procedure Procedure_Renaming_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Procedure_Renaming_Declarations
         .Procedure_Renaming_Declaration_Access);

   overriding procedure Function_Renaming_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Function_Renaming_Declarations
         .Function_Renaming_Declaration_Access);

   overriding procedure Package_Renaming_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Package_Renaming_Declarations
         .Package_Renaming_Declaration_Access);

   overriding procedure Generic_Package_Renaming_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Generic_Package_Renaming_Declarations
         .Generic_Package_Renaming_Declaration_Access);

   overriding procedure Generic_Procedure_Renaming_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements
         .Generic_Procedure_Renaming_Declarations
         .Generic_Procedure_Renaming_Declaration_Access);

   overriding procedure Generic_Function_Renaming_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Generic_Function_Renaming_Declarations
         .Generic_Function_Renaming_Declaration_Access);

   overriding procedure Task_Body_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Task_Body_Declarations
         .Task_Body_Declaration_Access);

   overriding procedure Protected_Body_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Protected_Body_Declarations
         .Protected_Body_Declaration_Access);

   overriding procedure Entry_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Entry_Declarations
         .Entry_Declaration_Access);

   overriding procedure Entry_Body_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Entry_Body_Declarations
         .Entry_Body_Declaration_Access);

   overriding procedure Entry_Index_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Entry_Index_Specifications
         .Entry_Index_Specification_Access);

   overriding procedure Procedure_Body_Stub
    (Self    : in out Visitor;
     Element : not null Program.Elements.Procedure_Body_Stubs
         .Procedure_Body_Stub_Access);

   overriding procedure Function_Body_Stub
    (Self    : in out Visitor;
     Element : not null Program.Elements.Function_Body_Stubs
         .Function_Body_Stub_Access);

   overriding procedure Package_Body_Stub
    (Self    : in out Visitor;
     Element : not null Program.Elements.Package_Body_Stubs
         .Package_Body_Stub_Access);

   overriding procedure Task_Body_Stub
    (Self    : in out Visitor;
     Element : not null Program.Elements.Task_Body_Stubs
         .Task_Body_Stub_Access);

   overriding procedure Protected_Body_Stub
    (Self    : in out Visitor;
     Element : not null Program.Elements.Protected_Body_Stubs
         .Protected_Body_Stub_Access);

   overriding procedure Exception_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Exception_Declarations
         .Exception_Declaration_Access);

   overriding procedure Choice_Parameter_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Choice_Parameter_Specifications
         .Choice_Parameter_Specification_Access);

   overriding procedure Generic_Package_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Generic_Package_Declarations
         .Generic_Package_Declaration_Access);

   overriding procedure Generic_Procedure_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Generic_Procedure_Declarations
         .Generic_Procedure_Declaration_Access);

   overriding procedure Generic_Function_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Generic_Function_Declarations
         .Generic_Function_Declaration_Access);

   overriding procedure Package_Instantiation
    (Self    : in out Visitor;
     Element : not null Program.Elements.Package_Instantiations
         .Package_Instantiation_Access);

   overriding procedure Procedure_Instantiation
    (Self    : in out Visitor;
     Element : not null Program.Elements.Procedure_Instantiations
         .Procedure_Instantiation_Access);

   overriding procedure Function_Instantiation
    (Self    : in out Visitor;
     Element : not null Program.Elements.Function_Instantiations
         .Function_Instantiation_Access);

   overriding procedure Formal_Object_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Object_Declarations
         .Formal_Object_Declaration_Access);

   overriding procedure Formal_Type_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Type_Declarations
         .Formal_Type_Declaration_Access);

   overriding procedure Formal_Procedure_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Procedure_Declarations
         .Formal_Procedure_Declaration_Access);

   overriding procedure Formal_Function_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Function_Declarations
         .Formal_Function_Declaration_Access);

   overriding procedure Formal_Package_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Package_Declarations
         .Formal_Package_Declaration_Access);

   overriding procedure Subtype_Indication
    (Self    : in out Visitor;
     Element : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access);

   overriding procedure Component_Definition
    (Self    : in out Visitor;
     Element : not null Program.Elements.Component_Definitions
         .Component_Definition_Access);

   overriding procedure Discrete_Subtype_Indication
    (Self    : in out Visitor;
     Element : not null Program.Elements.Discrete_Subtype_Indications
         .Discrete_Subtype_Indication_Access);

   overriding procedure Discrete_Range_Attribute_Reference
    (Self    : in out Visitor;
     Element : not null Program.Elements.Discrete_Range_Attribute_References
         .Discrete_Range_Attribute_Reference_Access);

   overriding procedure Discrete_Simple_Expression_Range
    (Self    : in out Visitor;
     Element : not null Program.Elements.Discrete_Simple_Expression_Ranges
         .Discrete_Simple_Expression_Range_Access);

   overriding procedure Known_Discriminant_Part
    (Self    : in out Visitor;
     Element : not null Program.Elements.Known_Discriminant_Parts
         .Known_Discriminant_Part_Access);

   overriding procedure Record_Definition
    (Self    : in out Visitor;
     Element : not null Program.Elements.Record_Definitions
         .Record_Definition_Access);

   overriding procedure Variant_Part
    (Self    : in out Visitor;
     Element : not null Program.Elements.Variant_Parts.Variant_Part_Access);

   overriding procedure Variant
    (Self    : in out Visitor;
     Element : not null Program.Elements.Variants.Variant_Access);

   overriding procedure Anonymous_Access_To_Object
    (Self    : in out Visitor;
     Element : not null Program.Elements.Anonymous_Access_To_Objects
         .Anonymous_Access_To_Object_Access);

   overriding procedure Anonymous_Access_To_Procedure
    (Self    : in out Visitor;
     Element : not null Program.Elements.Anonymous_Access_To_Procedures
         .Anonymous_Access_To_Procedure_Access);

   overriding procedure Anonymous_Access_To_Function
    (Self    : in out Visitor;
     Element : not null Program.Elements.Anonymous_Access_To_Functions
         .Anonymous_Access_To_Function_Access);

   overriding procedure Private_Extension_Definition
    (Self    : in out Visitor;
     Element : not null Program.Elements.Private_Extension_Definitions
         .Private_Extension_Definition_Access);

   overriding procedure Task_Definition
    (Self    : in out Visitor;
     Element : not null Program.Elements.Task_Definitions
         .Task_Definition_Access);

   overriding procedure Protected_Definition
    (Self    : in out Visitor;
     Element : not null Program.Elements.Protected_Definitions
         .Protected_Definition_Access);

   overriding procedure Aspect_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Access);

   overriding procedure Real_Range_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access);

   overriding procedure Explicit_Dereference
    (Self    : in out Visitor;
     Element : not null Program.Elements.Explicit_Dereferences
         .Explicit_Dereference_Access);

   overriding procedure Function_Call
    (Self    : in out Visitor;
     Element : not null Program.Elements.Function_Calls.Function_Call_Access);

   overriding procedure Indexed_Component
    (Self    : in out Visitor;
     Element : not null Program.Elements.Indexed_Components
         .Indexed_Component_Access);

   overriding procedure Slice
    (Self    : in out Visitor;
     Element : not null Program.Elements.Slices.Slice_Access);

   overriding procedure Selected_Component
    (Self    : in out Visitor;
     Element : not null Program.Elements.Selected_Components
         .Selected_Component_Access);

   overriding procedure Attribute_Reference
    (Self    : in out Visitor;
     Element : not null Program.Elements.Attribute_References
         .Attribute_Reference_Access);

   overriding procedure Record_Aggregate
    (Self    : in out Visitor;
     Element : not null Program.Elements.Record_Aggregates
         .Record_Aggregate_Access);

   overriding procedure Extension_Aggregate
    (Self    : in out Visitor;
     Element : not null Program.Elements.Extension_Aggregates
         .Extension_Aggregate_Access);

   overriding procedure Array_Aggregate
    (Self    : in out Visitor;
     Element : not null Program.Elements.Array_Aggregates
         .Array_Aggregate_Access);

   overriding procedure Short_Circuit_Operation
    (Self    : in out Visitor;
     Element : not null Program.Elements.Short_Circuit_Operations
         .Short_Circuit_Operation_Access);

   overriding procedure Membership_Test
    (Self    : in out Visitor;
     Element : not null Program.Elements.Membership_Tests
         .Membership_Test_Access);

   overriding procedure Parenthesized_Expression
    (Self    : in out Visitor;
     Element : not null Program.Elements.Parenthesized_Expressions
         .Parenthesized_Expression_Access);

   overriding procedure Raise_Expression
    (Self    : in out Visitor;
     Element : not null Program.Elements.Raise_Expressions
         .Raise_Expression_Access);

   overriding procedure Type_Conversion
    (Self    : in out Visitor;
     Element : not null Program.Elements.Type_Conversions
         .Type_Conversion_Access);

   overriding procedure Qualified_Expression
    (Self    : in out Visitor;
     Element : not null Program.Elements.Qualified_Expressions
         .Qualified_Expression_Access);

   overriding procedure Allocator
    (Self    : in out Visitor;
     Element : not null Program.Elements.Allocators.Allocator_Access);

   overriding procedure Case_Expression
    (Self    : in out Visitor;
     Element : not null Program.Elements.Case_Expressions
         .Case_Expression_Access);

   overriding procedure If_Expression
    (Self    : in out Visitor;
     Element : not null Program.Elements.If_Expressions.If_Expression_Access);

   overriding procedure Quantified_Expression
    (Self    : in out Visitor;
     Element : not null Program.Elements.Quantified_Expressions
         .Quantified_Expression_Access);

   overriding procedure Discriminant_Association
    (Self    : in out Visitor;
     Element : not null Program.Elements.Discriminant_Associations
         .Discriminant_Association_Access);

   overriding procedure Record_Component_Association
    (Self    : in out Visitor;
     Element : not null Program.Elements.Record_Component_Associations
         .Record_Component_Association_Access);

   overriding procedure Array_Component_Association
    (Self    : in out Visitor;
     Element : not null Program.Elements.Array_Component_Associations
         .Array_Component_Association_Access);

   overriding procedure Parameter_Association
    (Self    : in out Visitor;
     Element : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Access);

   overriding procedure Formal_Package_Association
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Package_Associations
         .Formal_Package_Association_Access);

   overriding procedure Assignment_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Assignment_Statements
         .Assignment_Statement_Access);

   overriding procedure If_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.If_Statements.If_Statement_Access);

   overriding procedure Case_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Case_Statements
         .Case_Statement_Access);

   overriding procedure Loop_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Loop_Statements
         .Loop_Statement_Access);

   overriding procedure While_Loop_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.While_Loop_Statements
         .While_Loop_Statement_Access);

   overriding procedure For_Loop_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.For_Loop_Statements
         .For_Loop_Statement_Access);

   overriding procedure Block_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Block_Statements
         .Block_Statement_Access);

   overriding procedure Exit_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Exit_Statements
         .Exit_Statement_Access);

   overriding procedure Goto_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Goto_Statements
         .Goto_Statement_Access);

   overriding procedure Call_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Call_Statements
         .Call_Statement_Access);

   overriding procedure Simple_Return_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Simple_Return_Statements
         .Simple_Return_Statement_Access);

   overriding procedure Extended_Return_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Extended_Return_Statements
         .Extended_Return_Statement_Access);

   overriding procedure Accept_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Accept_Statements
         .Accept_Statement_Access);

   overriding procedure Requeue_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Requeue_Statements
         .Requeue_Statement_Access);

   overriding procedure Delay_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Delay_Statements
         .Delay_Statement_Access);

   overriding procedure Select_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Select_Statements
         .Select_Statement_Access);

   overriding procedure Abort_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Abort_Statements
         .Abort_Statement_Access);

   overriding procedure Raise_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Raise_Statements
         .Raise_Statement_Access);

   overriding procedure Code_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Code_Statements
         .Code_Statement_Access);

   overriding procedure Elsif_Path
    (Self    : in out Visitor;
     Element : not null Program.Elements.Elsif_Paths.Elsif_Path_Access);

   overriding procedure Case_Path
    (Self    : in out Visitor;
     Element : not null Program.Elements.Case_Paths.Case_Path_Access);

   overriding procedure Select_Path
    (Self    : in out Visitor;
     Element : not null Program.Elements.Select_Paths.Select_Path_Access);

   overriding procedure Case_Expression_Path
    (Self    : in out Visitor;
     Element : not null Program.Elements.Case_Expression_Paths
         .Case_Expression_Path_Access);

   overriding procedure Elsif_Expression_Path
    (Self    : in out Visitor;
     Element : not null Program.Elements.Elsif_Expression_Paths
         .Elsif_Expression_Path_Access);

   overriding procedure Use_Clause
    (Self    : in out Visitor;
     Element : not null Program.Elements.Use_Clauses.Use_Clause_Access);

   overriding procedure With_Clause
    (Self    : in out Visitor;
     Element : not null Program.Elements.With_Clauses.With_Clause_Access);

   overriding procedure Component_Clause
    (Self    : in out Visitor;
     Element : not null Program.Elements.Component_Clauses
         .Component_Clause_Access);

   overriding procedure Derived_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Derived_Types.Derived_Type_Access);

   overriding procedure Derived_Record_Extension
    (Self    : in out Visitor;
     Element : not null Program.Elements.Derived_Record_Extensions
         .Derived_Record_Extension_Access);

   overriding procedure Enumeration_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Enumeration_Types
         .Enumeration_Type_Access);

   overriding procedure Signed_Integer_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Signed_Integer_Types
         .Signed_Integer_Type_Access);

   overriding procedure Modular_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Modular_Types.Modular_Type_Access);

   overriding procedure Floating_Point_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Floating_Point_Types
         .Floating_Point_Type_Access);

   overriding procedure Ordinary_Fixed_Point_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Ordinary_Fixed_Point_Types
         .Ordinary_Fixed_Point_Type_Access);

   overriding procedure Decimal_Fixed_Point_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Decimal_Fixed_Point_Types
         .Decimal_Fixed_Point_Type_Access);

   overriding procedure Unconstrained_Array_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Unconstrained_Array_Types
         .Unconstrained_Array_Type_Access);

   overriding procedure Constrained_Array_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Constrained_Array_Types
         .Constrained_Array_Type_Access);

   overriding procedure Record_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Record_Types.Record_Type_Access);

   overriding procedure Interface_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Interface_Types
         .Interface_Type_Access);

   overriding procedure Object_Access_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Object_Access_Types
         .Object_Access_Type_Access);

   overriding procedure Procedure_Access_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Procedure_Access_Types
         .Procedure_Access_Type_Access);

   overriding procedure Function_Access_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Function_Access_Types
         .Function_Access_Type_Access);

   overriding procedure Formal_Derived_Type_Definition
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Derived_Type_Definitions
         .Formal_Derived_Type_Definition_Access);

   overriding procedure Formal_Unconstrained_Array_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Unconstrained_Array_Types
         .Formal_Unconstrained_Array_Type_Access);

   overriding procedure Formal_Constrained_Array_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Constrained_Array_Types
         .Formal_Constrained_Array_Type_Access);

   overriding procedure Formal_Object_Access_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Object_Access_Types
         .Formal_Object_Access_Type_Access);

   overriding procedure Formal_Procedure_Access_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Procedure_Access_Types
         .Formal_Procedure_Access_Type_Access);

   overriding procedure Formal_Function_Access_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Function_Access_Types
         .Formal_Function_Access_Type_Access);

   overriding procedure Formal_Interface_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Interface_Types
         .Formal_Interface_Type_Access);

   overriding procedure Range_Attribute_Reference
    (Self    : in out Visitor;
     Element : not null Program.Elements.Range_Attribute_References
         .Range_Attribute_Reference_Access);

   overriding procedure Simple_Expression_Range
    (Self    : in out Visitor;
     Element : not null Program.Elements.Simple_Expression_Ranges
         .Simple_Expression_Range_Access);

   overriding procedure Digits_Constraint
    (Self    : in out Visitor;
     Element : not null Program.Elements.Digits_Constraints
         .Digits_Constraint_Access);

   overriding procedure Delta_Constraint
    (Self    : in out Visitor;
     Element : not null Program.Elements.Delta_Constraints
         .Delta_Constraint_Access);

   overriding procedure Index_Constraint
    (Self    : in out Visitor;
     Element : not null Program.Elements.Index_Constraints
         .Index_Constraint_Access);

   overriding procedure Discriminant_Constraint
    (Self    : in out Visitor;
     Element : not null Program.Elements.Discriminant_Constraints
         .Discriminant_Constraint_Access);

   overriding procedure Attribute_Definition_Clause
    (Self    : in out Visitor;
     Element : not null Program.Elements.Attribute_Definition_Clauses
         .Attribute_Definition_Clause_Access);

   overriding procedure Enumeration_Representation_Clause
    (Self    : in out Visitor;
     Element : not null Program.Elements.Enumeration_Representation_Clauses
         .Enumeration_Representation_Clause_Access);

   overriding procedure Record_Representation_Clause
    (Self    : in out Visitor;
     Element : not null Program.Elements.Record_Representation_Clauses
         .Record_Representation_Clause_Access);

   overriding procedure At_Clause
    (Self    : in out Visitor;
     Element : not null Program.Elements.At_Clauses.At_Clause_Access);

   overriding procedure Exception_Handler
    (Self    : in out Visitor;
     Element : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Access);

   function F1_1 is new Generic_Child
     (Element      => Program.Elements.Pragmas.Pragma_Element,
      Child        => Program.Elements.Identifiers.Identifier,
      Child_Access => Program.Elements.Identifiers.Identifier_Access,
      Get_Child    => Program.Elements.Pragmas.Name);

   function F1_2 is new Generic_Vector
     (Parent        => Program.Elements.Pragmas.Pragma_Element,
      Vector        =>
        Program.Elements.Parameter_Associations.Parameter_Association_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access,
      Get_Vector    => Program.Elements.Pragmas.Arguments);

   F1   : aliased constant Getter_Array :=
     (1 => (False, Name, F1_1'Access),
      2 => (True, Arguments, F1_2'Access));

   overriding procedure Pragma_Element
    (Self    : in out Visitor;
     Element : not null Program.Elements.Pragmas.Pragma_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F1'Access;
   end Pragma_Element;

   function F5_1 is new Generic_Child
     (Element      =>
        Program.Elements.Defining_Expanded_Names.Defining_Expanded_Name,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Defining_Expanded_Names.Prefix);

   function F5_2 is new Generic_Child
     (Element      =>
        Program.Elements.Defining_Expanded_Names.Defining_Expanded_Name,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Defining_Expanded_Names.Selector);

   F5   : aliased constant Getter_Array :=
     (1 => (False, Prefix, F5_1'Access),
      2 => (False, Selector, F5_2'Access));

   overriding procedure Defining_Expanded_Name
    (Self    : in out Visitor;
     Element : not null Program.Elements.Defining_Expanded_Names
         .Defining_Expanded_Name_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F5'Access;
   end Defining_Expanded_Name;

   function F6_1 is new Generic_Child
     (Element      => Program.Elements.Type_Declarations.Type_Declaration,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Type_Declarations.Name);

   function F6_2 is new Generic_Child
     (Element      => Program.Elements.Type_Declarations.Type_Declaration,
      Child        => Program.Elements.Definitions.Definition,
      Child_Access => Program.Elements.Definitions.Definition_Access,
      Get_Child    => Program.Elements.Type_Declarations.Discriminant_Part);

   function F6_3 is new Generic_Child
     (Element      => Program.Elements.Type_Declarations.Type_Declaration,
      Child        => Program.Elements.Definitions.Definition,
      Child_Access => Program.Elements.Definitions.Definition_Access,
      Get_Child    => Program.Elements.Type_Declarations.Definition);

   function F6_4 is new Generic_Vector
     (Parent        => Program.Elements.Type_Declarations.Type_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Type_Declarations.Aspects);

   F6   : aliased constant Getter_Array :=
     (1 => (False, Name, F6_1'Access),
      2 => (False, Discriminant_Part, F6_2'Access),
      3 => (False, Definition, F6_3'Access),
      4 => (True, Aspects, F6_4'Access));

   overriding procedure Type_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Type_Declarations
         .Type_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F6'Access;
   end Type_Declaration;

   function F7_1 is new Generic_Child
     (Element      =>
        Program.Elements.Task_Type_Declarations.Task_Type_Declaration,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Task_Type_Declarations.Name);

   function F7_2 is new Generic_Child
     (Element      =>
        Program.Elements.Task_Type_Declarations.Task_Type_Declaration,
      Child        =>
        Program.Elements.Known_Discriminant_Parts.Known_Discriminant_Part,
      Child_Access =>
        Program.Elements.Known_Discriminant_Parts
          .Known_Discriminant_Part_Access,
      Get_Child    =>
        Program.Elements.Task_Type_Declarations.Discriminant_Part);

   function F7_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Task_Type_Declarations.Task_Type_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Task_Type_Declarations.Aspects);

   function F7_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Task_Type_Declarations.Task_Type_Declaration,
      Vector        => Program.Elements.Expressions.Expression_Vector,
      Vector_Access => Program.Elements.Expressions.Expression_Vector_Access,
      Get_Vector    => Program.Elements.Task_Type_Declarations.Progenitors);

   function F7_5 is new Generic_Child
     (Element      =>
        Program.Elements.Task_Type_Declarations.Task_Type_Declaration,
      Child        => Program.Elements.Task_Definitions.Task_Definition,
      Child_Access => Program.Elements.Task_Definitions.Task_Definition_Access,
      Get_Child    => Program.Elements.Task_Type_Declarations.Definition);

   F7   : aliased constant Getter_Array :=
     (1 => (False, Name, F7_1'Access),
      2 => (False, Discriminant_Part, F7_2'Access),
      3 => (True, Aspects, F7_3'Access),
      4 => (True, Progenitors, F7_4'Access),
      5 => (False, Definition, F7_5'Access));

   overriding procedure Task_Type_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Task_Type_Declarations
         .Task_Type_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F7'Access;
   end Task_Type_Declaration;

   function F8_1 is new Generic_Child
     (Element      =>
        Program.Elements.Protected_Type_Declarations
          .Protected_Type_Declaration,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Protected_Type_Declarations.Name);

   function F8_2 is new Generic_Child
     (Element      =>
        Program.Elements.Protected_Type_Declarations
          .Protected_Type_Declaration,
      Child        =>
        Program.Elements.Known_Discriminant_Parts.Known_Discriminant_Part,
      Child_Access =>
        Program.Elements.Known_Discriminant_Parts
          .Known_Discriminant_Part_Access,
      Get_Child    =>
        Program.Elements.Protected_Type_Declarations.Discriminant_Part);

   function F8_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Protected_Type_Declarations
          .Protected_Type_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Protected_Type_Declarations.Aspects);

   function F8_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Protected_Type_Declarations
          .Protected_Type_Declaration,
      Vector        => Program.Elements.Expressions.Expression_Vector,
      Vector_Access => Program.Elements.Expressions.Expression_Vector_Access,
      Get_Vector    =>
        Program.Elements.Protected_Type_Declarations.Progenitors);

   function F8_5 is new Generic_Child
     (Element      =>
        Program.Elements.Protected_Type_Declarations
          .Protected_Type_Declaration,
      Child        =>
        Program.Elements.Protected_Definitions.Protected_Definition,
      Child_Access =>
        Program.Elements.Protected_Definitions.Protected_Definition_Access,
      Get_Child    => Program.Elements.Protected_Type_Declarations.Definition);

   F8   : aliased constant Getter_Array :=
     (1 => (False, Name, F8_1'Access),
      2 => (False, Discriminant_Part, F8_2'Access),
      3 => (True, Aspects, F8_3'Access),
      4 => (True, Progenitors, F8_4'Access),
      5 => (False, Definition, F8_5'Access));

   overriding procedure Protected_Type_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Protected_Type_Declarations
         .Protected_Type_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F8'Access;
   end Protected_Type_Declaration;

   function F9_1 is new Generic_Child
     (Element      =>
        Program.Elements.Subtype_Declarations.Subtype_Declaration,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Subtype_Declarations.Name);

   function F9_2 is new Generic_Child
     (Element      =>
        Program.Elements.Subtype_Declarations.Subtype_Declaration,
      Child        => Program.Elements.Subtype_Indications.Subtype_Indication,
      Child_Access =>
        Program.Elements.Subtype_Indications.Subtype_Indication_Access,
      Get_Child    =>
        Program.Elements.Subtype_Declarations.Subtype_Indication);

   function F9_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Subtype_Declarations.Subtype_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Subtype_Declarations.Aspects);

   F9   : aliased constant Getter_Array :=
     (1 => (False, Name, F9_1'Access),
      2 => (False, Subtype_Indication, F9_2'Access),
      3 => (True, Aspects, F9_3'Access));

   overriding procedure Subtype_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Subtype_Declarations
         .Subtype_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F9'Access;
   end Subtype_Declaration;

   function F10_1 is new Generic_Vector
     (Parent        => Program.Elements.Object_Declarations.Object_Declaration,
      Vector        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Vector,
      Vector_Access =>
        Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access,
      Get_Vector    => Program.Elements.Object_Declarations.Names);

   function F10_2 is new Generic_Child
     (Element      => Program.Elements.Object_Declarations.Object_Declaration,
      Child        => Program.Elements.Definitions.Definition,
      Child_Access => Program.Elements.Definitions.Definition_Access,
      Get_Child    => Program.Elements.Object_Declarations.Object_Subtype);

   function F10_3 is new Generic_Child
     (Element      => Program.Elements.Object_Declarations.Object_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Object_Declarations.Initialization_Expression);

   function F10_4 is new Generic_Vector
     (Parent        => Program.Elements.Object_Declarations.Object_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Object_Declarations.Aspects);

   F10  : aliased constant Getter_Array :=
     (1 => (True, Names, F10_1'Access),
      2 => (False, Object_Subtype, F10_2'Access),
      3 =>
        (False,
         Initialization_Expression,
         F10_3'Access),
      4 => (True, Aspects, F10_4'Access));

   overriding procedure Object_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Object_Declarations
         .Object_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F10'Access;
   end Object_Declaration;

   function F11_1 is new Generic_Child
     (Element      =>
        Program.Elements.Single_Task_Declarations.Single_Task_Declaration,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Single_Task_Declarations.Name);

   function F11_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Single_Task_Declarations.Single_Task_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Single_Task_Declarations.Aspects);

   function F11_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Single_Task_Declarations.Single_Task_Declaration,
      Vector        => Program.Elements.Expressions.Expression_Vector,
      Vector_Access => Program.Elements.Expressions.Expression_Vector_Access,
      Get_Vector    => Program.Elements.Single_Task_Declarations.Progenitors);

   function F11_4 is new Generic_Child
     (Element      =>
        Program.Elements.Single_Task_Declarations.Single_Task_Declaration,
      Child        => Program.Elements.Task_Definitions.Task_Definition,
      Child_Access => Program.Elements.Task_Definitions.Task_Definition_Access,
      Get_Child    => Program.Elements.Single_Task_Declarations.Definition);

   F11  : aliased constant Getter_Array :=
     (1 => (False, Name, F11_1'Access),
      2 => (True, Aspects, F11_2'Access),
      3 => (True, Progenitors, F11_3'Access),
      4 => (False, Definition, F11_4'Access));

   overriding procedure Single_Task_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Single_Task_Declarations
         .Single_Task_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F11'Access;
   end Single_Task_Declaration;

   function F12_1 is new Generic_Child
     (Element      =>
        Program.Elements.Single_Protected_Declarations
          .Single_Protected_Declaration,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Single_Protected_Declarations.Name);

   function F12_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Single_Protected_Declarations
          .Single_Protected_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Single_Protected_Declarations.Aspects);

   function F12_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Single_Protected_Declarations
          .Single_Protected_Declaration,
      Vector        => Program.Elements.Expressions.Expression_Vector,
      Vector_Access => Program.Elements.Expressions.Expression_Vector_Access,
      Get_Vector    =>
        Program.Elements.Single_Protected_Declarations.Progenitors);

   function F12_4 is new Generic_Child
     (Element      =>
        Program.Elements.Single_Protected_Declarations
          .Single_Protected_Declaration,
      Child        =>
        Program.Elements.Protected_Definitions.Protected_Definition,
      Child_Access =>
        Program.Elements.Protected_Definitions.Protected_Definition_Access,
      Get_Child    =>
        Program.Elements.Single_Protected_Declarations.Definition);

   F12  : aliased constant Getter_Array :=
     (1 => (False, Name, F12_1'Access),
      2 => (True, Aspects, F12_2'Access),
      3 => (True, Progenitors, F12_3'Access),
      4 => (False, Definition, F12_4'Access));

   overriding procedure Single_Protected_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Single_Protected_Declarations
         .Single_Protected_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F12'Access;
   end Single_Protected_Declaration;

   function F13_1 is new Generic_Vector
     (Parent        => Program.Elements.Number_Declarations.Number_Declaration,
      Vector        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Vector,
      Vector_Access =>
        Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access,
      Get_Vector    => Program.Elements.Number_Declarations.Names);

   function F13_2 is new Generic_Child
     (Element      => Program.Elements.Number_Declarations.Number_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Number_Declarations.Expression);

   F13  : aliased constant Getter_Array :=
     (1 => (True, Names, F13_1'Access),
      2 => (False, Expression, F13_2'Access));

   overriding procedure Number_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Number_Declarations
         .Number_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F13'Access;
   end Number_Declaration;

   function F14_1 is new Generic_Child
     (Element      =>
        Program.Elements.Enumeration_Literal_Specifications
          .Enumeration_Literal_Specification,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    =>
        Program.Elements.Enumeration_Literal_Specifications.Name);

   F14  : aliased constant Getter_Array :=
     (1 => (False, Name, F14_1'Access));

   overriding procedure Enumeration_Literal_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Enumeration_Literal_Specifications
         .Enumeration_Literal_Specification_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F14'Access;
   end Enumeration_Literal_Specification;

   function F15_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Discriminant_Specifications
          .Discriminant_Specification,
      Vector        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Vector,
      Vector_Access =>
        Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access,
      Get_Vector    => Program.Elements.Discriminant_Specifications.Names);

   function F15_2 is new Generic_Child
     (Element      =>
        Program.Elements.Discriminant_Specifications
          .Discriminant_Specification,
      Child        => Program.Elements.Element,
      Child_Access => Program.Elements.Element_Access,
      Get_Child    =>
        Program.Elements.Discriminant_Specifications.Object_Subtype);

   function F15_3 is new Generic_Child
     (Element      =>
        Program.Elements.Discriminant_Specifications
          .Discriminant_Specification,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Discriminant_Specifications.Default_Expression);

   F15  : aliased constant Getter_Array :=
     (1 => (True, Names, F15_1'Access),
      2 => (False, Object_Subtype, F15_2'Access),
      3 => (False, Default_Expression, F15_3'Access));

   overriding procedure Discriminant_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Discriminant_Specifications
         .Discriminant_Specification_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F15'Access;
   end Discriminant_Specification;

   function F16_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Component_Declarations.Component_Declaration,
      Vector        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Vector,
      Vector_Access =>
        Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access,
      Get_Vector    => Program.Elements.Component_Declarations.Names);

   function F16_2 is new Generic_Child
     (Element      =>
        Program.Elements.Component_Declarations.Component_Declaration,
      Child        =>
        Program.Elements.Component_Definitions.Component_Definition,
      Child_Access =>
        Program.Elements.Component_Definitions.Component_Definition_Access,
      Get_Child    => Program.Elements.Component_Declarations.Object_Subtype);

   function F16_3 is new Generic_Child
     (Element      =>
        Program.Elements.Component_Declarations.Component_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Component_Declarations.Default_Expression);

   function F16_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Component_Declarations.Component_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Component_Declarations.Aspects);

   F16  : aliased constant Getter_Array :=
     (1 => (True, Names, F16_1'Access),
      2 => (False, Object_Subtype, F16_2'Access),
      3 => (False, Default_Expression, F16_3'Access),
      4 => (True, Aspects, F16_4'Access));

   overriding procedure Component_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Component_Declarations
         .Component_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F16'Access;
   end Component_Declaration;

   function F17_1 is new Generic_Child
     (Element      =>
        Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Loop_Parameter_Specifications.Name);

   function F17_2 is new Generic_Child
     (Element      =>
        Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification,
      Child        => Program.Elements.Discrete_Ranges.Discrete_Range,
      Child_Access => Program.Elements.Discrete_Ranges.Discrete_Range_Access,
      Get_Child    =>
        Program.Elements.Loop_Parameter_Specifications.Definition);

   F17  : aliased constant Getter_Array :=
     (1 => (False, Name, F17_1'Access),
      2 => (False, Definition, F17_2'Access));

   overriding procedure Loop_Parameter_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Loop_Parameter_Specifications
         .Loop_Parameter_Specification_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F17'Access;
   end Loop_Parameter_Specification;

   function F18_1 is new Generic_Child
     (Element      =>
        Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    =>
        Program.Elements.Generalized_Iterator_Specifications.Name);

   function F18_2 is new Generic_Child
     (Element      =>
        Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Generalized_Iterator_Specifications.Iterator_Name);

   F18  : aliased constant Getter_Array :=
     (1 => (False, Name, F18_1'Access),
      2 => (False, Iterator_Name, F18_2'Access));

   overriding procedure Generalized_Iterator_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Generalized_Iterator_Specifications
         .Generalized_Iterator_Specification_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F18'Access;
   end Generalized_Iterator_Specification;

   function F19_1 is new Generic_Child
     (Element      =>
        Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Element_Iterator_Specifications.Name);

   function F19_2 is new Generic_Child
     (Element      =>
        Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification,
      Child        => Program.Elements.Subtype_Indications.Subtype_Indication,
      Child_Access =>
        Program.Elements.Subtype_Indications.Subtype_Indication_Access,
      Get_Child    =>
        Program.Elements.Element_Iterator_Specifications.Subtype_Indication);

   function F19_3 is new Generic_Child
     (Element      =>
        Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Element_Iterator_Specifications.Iterable_Name);

   F19  : aliased constant Getter_Array :=
     (1 => (False, Name, F19_1'Access),
      2 => (False, Subtype_Indication, F19_2'Access),
      3 => (False, Iterable_Name, F19_3'Access));

   overriding procedure Element_Iterator_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Element_Iterator_Specifications
         .Element_Iterator_Specification_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F19'Access;
   end Element_Iterator_Specification;

   function F20_1 is new Generic_Child
     (Element      =>
        Program.Elements.Procedure_Declarations.Procedure_Declaration,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    => Program.Elements.Procedure_Declarations.Name);

   function F20_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Procedure_Declarations.Procedure_Declaration,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Procedure_Declarations.Parameters);

   function F20_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Procedure_Declarations.Procedure_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Procedure_Declarations.Aspects);

   F20  : aliased constant Getter_Array :=
     (1 => (False, Name, F20_1'Access),
      2 => (True, Parameters, F20_2'Access),
      3 => (True, Aspects, F20_3'Access));

   overriding procedure Procedure_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Procedure_Declarations
         .Procedure_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F20'Access;
   end Procedure_Declaration;

   function F21_1 is new Generic_Child
     (Element      =>
        Program.Elements.Function_Declarations.Function_Declaration,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    => Program.Elements.Function_Declarations.Name);

   function F21_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Function_Declarations.Function_Declaration,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Function_Declarations.Parameters);

   function F21_3 is new Generic_Child
     (Element      =>
        Program.Elements.Function_Declarations.Function_Declaration,
      Child        => Program.Elements.Element,
      Child_Access => Program.Elements.Element_Access,
      Get_Child    => Program.Elements.Function_Declarations.Result_Subtype);

   function F21_4 is new Generic_Child
     (Element      =>
        Program.Elements.Function_Declarations.Function_Declaration,
      Child        =>
        Program.Elements.Parenthesized_Expressions.Parenthesized_Expression,
      Child_Access =>
        Program.Elements.Parenthesized_Expressions
          .Parenthesized_Expression_Access,
      Get_Child    =>
        Program.Elements.Function_Declarations.Result_Expression);

   function F21_5 is new Generic_Vector
     (Parent        =>
        Program.Elements.Function_Declarations.Function_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Function_Declarations.Aspects);

   F21  : aliased constant Getter_Array :=
     (1 => (False, Name, F21_1'Access),
      2 => (True, Parameters, F21_2'Access),
      3 => (False, Result_Subtype, F21_3'Access),
      4 => (False, Result_Expression, F21_4'Access),
      5 => (True, Aspects, F21_5'Access));

   overriding procedure Function_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Function_Declarations
         .Function_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F21'Access;
   end Function_Declaration;

   function F22_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Parameter_Specifications.Parameter_Specification,
      Vector        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Vector,
      Vector_Access =>
        Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access,
      Get_Vector    => Program.Elements.Parameter_Specifications.Names);

   function F22_2 is new Generic_Child
     (Element      =>
        Program.Elements.Parameter_Specifications.Parameter_Specification,
      Child        => Program.Elements.Element,
      Child_Access => Program.Elements.Element_Access,
      Get_Child    =>
        Program.Elements.Parameter_Specifications.Parameter_Subtype);

   function F22_3 is new Generic_Child
     (Element      =>
        Program.Elements.Parameter_Specifications.Parameter_Specification,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Parameter_Specifications.Default_Expression);

   F22  : aliased constant Getter_Array :=
     (1 => (True, Names, F22_1'Access),
      2 => (False, Parameter_Subtype, F22_2'Access),
      3 => (False, Default_Expression, F22_3'Access));

   overriding procedure Parameter_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F22'Access;
   end Parameter_Specification;

   function F23_1 is new Generic_Child
     (Element      =>
        Program.Elements.Procedure_Body_Declarations
          .Procedure_Body_Declaration,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    => Program.Elements.Procedure_Body_Declarations.Name);

   function F23_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Procedure_Body_Declarations
          .Procedure_Body_Declaration,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Procedure_Body_Declarations.Parameters);

   function F23_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Procedure_Body_Declarations
          .Procedure_Body_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Procedure_Body_Declarations.Aspects);

   function F23_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Procedure_Body_Declarations
          .Procedure_Body_Declaration,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    =>
        Program.Elements.Procedure_Body_Declarations.Declarations);

   function F23_5 is new Generic_Vector
     (Parent        =>
        Program.Elements.Procedure_Body_Declarations
          .Procedure_Body_Declaration,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    =>
        Program.Elements.Procedure_Body_Declarations.Statements);

   function F23_6 is new Generic_Vector
     (Parent        =>
        Program.Elements.Procedure_Body_Declarations
          .Procedure_Body_Declaration,
      Vector        =>
        Program.Elements.Exception_Handlers.Exception_Handler_Vector,
      Vector_Access =>
        Program.Elements.Exception_Handlers.Exception_Handler_Vector_Access,
      Get_Vector    =>
        Program.Elements.Procedure_Body_Declarations.Exception_Handlers);

   function F23_7 is new Generic_Child
     (Element      =>
        Program.Elements.Procedure_Body_Declarations
          .Procedure_Body_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Procedure_Body_Declarations.End_Name);

   F23  : aliased constant Getter_Array :=
     (1 => (False, Name, F23_1'Access),
      2 => (True, Parameters, F23_2'Access),
      3 => (True, Aspects, F23_3'Access),
      4 => (True, Declarations, F23_4'Access),
      5 => (True, Statements, F23_5'Access),
      6 => (True, Exception_Handlers, F23_6'Access),
      7 => (False, End_Name, F23_7'Access));

   overriding procedure Procedure_Body_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Procedure_Body_Declarations
         .Procedure_Body_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F23'Access;
   end Procedure_Body_Declaration;

   function F24_1 is new Generic_Child
     (Element      =>
        Program.Elements.Function_Body_Declarations.Function_Body_Declaration,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    => Program.Elements.Function_Body_Declarations.Name);

   function F24_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Function_Body_Declarations.Function_Body_Declaration,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Function_Body_Declarations.Parameters);

   function F24_3 is new Generic_Child
     (Element      =>
        Program.Elements.Function_Body_Declarations.Function_Body_Declaration,
      Child        => Program.Elements.Element,
      Child_Access => Program.Elements.Element_Access,
      Get_Child    =>
        Program.Elements.Function_Body_Declarations.Result_Subtype);

   function F24_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Function_Body_Declarations.Function_Body_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Function_Body_Declarations.Aspects);

   function F24_5 is new Generic_Vector
     (Parent        =>
        Program.Elements.Function_Body_Declarations.Function_Body_Declaration,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    =>
        Program.Elements.Function_Body_Declarations.Declarations);

   function F24_6 is new Generic_Vector
     (Parent        =>
        Program.Elements.Function_Body_Declarations.Function_Body_Declaration,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Function_Body_Declarations.Statements);

   function F24_7 is new Generic_Vector
     (Parent        =>
        Program.Elements.Function_Body_Declarations.Function_Body_Declaration,
      Vector        =>
        Program.Elements.Exception_Handlers.Exception_Handler_Vector,
      Vector_Access =>
        Program.Elements.Exception_Handlers.Exception_Handler_Vector_Access,
      Get_Vector    =>
        Program.Elements.Function_Body_Declarations.Exception_Handlers);

   function F24_8 is new Generic_Child
     (Element      =>
        Program.Elements.Function_Body_Declarations.Function_Body_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Function_Body_Declarations.End_Name);

   F24  : aliased constant Getter_Array :=
     (1 => (False, Name, F24_1'Access),
      2 => (True, Parameters, F24_2'Access),
      3 => (False, Result_Subtype, F24_3'Access),
      4 => (True, Aspects, F24_4'Access),
      5 => (True, Declarations, F24_5'Access),
      6 => (True, Statements, F24_6'Access),
      7 => (True, Exception_Handlers, F24_7'Access),
      8 => (False, End_Name, F24_8'Access));

   overriding procedure Function_Body_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Function_Body_Declarations
         .Function_Body_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F24'Access;
   end Function_Body_Declaration;

   function F25_1 is new Generic_Child
     (Element      =>
        Program.Elements.Return_Object_Specifications
          .Return_Object_Specification,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Return_Object_Specifications.Name);

   function F25_2 is new Generic_Child
     (Element      =>
        Program.Elements.Return_Object_Specifications
          .Return_Object_Specification,
      Child        => Program.Elements.Element,
      Child_Access => Program.Elements.Element_Access,
      Get_Child    =>
        Program.Elements.Return_Object_Specifications.Object_Subtype);

   function F25_3 is new Generic_Child
     (Element      =>
        Program.Elements.Return_Object_Specifications
          .Return_Object_Specification,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Return_Object_Specifications.Expression);

   F25  : aliased constant Getter_Array :=
     (1 => (False, Name, F25_1'Access),
      2 => (False, Object_Subtype, F25_2'Access),
      3 => (False, Expression, F25_3'Access));

   overriding procedure Return_Object_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Return_Object_Specifications
         .Return_Object_Specification_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F25'Access;
   end Return_Object_Specification;

   function F26_1 is new Generic_Child
     (Element      =>
        Program.Elements.Package_Declarations.Package_Declaration,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    => Program.Elements.Package_Declarations.Name);

   function F26_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Package_Declarations.Package_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Package_Declarations.Aspects);

   function F26_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Package_Declarations.Package_Declaration,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    =>
        Program.Elements.Package_Declarations.Visible_Declarations);

   function F26_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Package_Declarations.Package_Declaration,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    =>
        Program.Elements.Package_Declarations.Private_Declarations);

   function F26_5 is new Generic_Child
     (Element      =>
        Program.Elements.Package_Declarations.Package_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Package_Declarations.End_Name);

   F26  : aliased constant Getter_Array :=
     (1 => (False, Name, F26_1'Access),
      2 => (True, Aspects, F26_2'Access),
      3 => (True, Visible_Declarations, F26_3'Access),
      4 => (True, Private_Declarations, F26_4'Access),
      5 => (False, End_Name, F26_5'Access));

   overriding procedure Package_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Package_Declarations
         .Package_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F26'Access;
   end Package_Declaration;

   function F27_1 is new Generic_Child
     (Element      =>
        Program.Elements.Package_Body_Declarations.Package_Body_Declaration,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    => Program.Elements.Package_Body_Declarations.Name);

   function F27_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Package_Body_Declarations.Package_Body_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Package_Body_Declarations.Aspects);

   function F27_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Package_Body_Declarations.Package_Body_Declaration,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    =>
        Program.Elements.Package_Body_Declarations.Declarations);

   function F27_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Package_Body_Declarations.Package_Body_Declaration,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Package_Body_Declarations.Statements);

   function F27_5 is new Generic_Vector
     (Parent        =>
        Program.Elements.Package_Body_Declarations.Package_Body_Declaration,
      Vector        =>
        Program.Elements.Exception_Handlers.Exception_Handler_Vector,
      Vector_Access =>
        Program.Elements.Exception_Handlers.Exception_Handler_Vector_Access,
      Get_Vector    =>
        Program.Elements.Package_Body_Declarations.Exception_Handlers);

   function F27_6 is new Generic_Child
     (Element      =>
        Program.Elements.Package_Body_Declarations.Package_Body_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Package_Body_Declarations.End_Name);

   F27  : aliased constant Getter_Array :=
     (1 => (False, Name, F27_1'Access),
      2 => (True, Aspects, F27_2'Access),
      3 => (True, Declarations, F27_3'Access),
      4 => (True, Statements, F27_4'Access),
      5 => (True, Exception_Handlers, F27_5'Access),
      6 => (False, End_Name, F27_6'Access));

   overriding procedure Package_Body_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Package_Body_Declarations
         .Package_Body_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F27'Access;
   end Package_Body_Declaration;

   function F28_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Object_Renaming_Declarations
          .Object_Renaming_Declaration,
      Vector        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Vector,
      Vector_Access =>
        Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access,
      Get_Vector    => Program.Elements.Object_Renaming_Declarations.Names);

   function F28_2 is new Generic_Child
     (Element      =>
        Program.Elements.Object_Renaming_Declarations
          .Object_Renaming_Declaration,
      Child        => Program.Elements.Element,
      Child_Access => Program.Elements.Element_Access,
      Get_Child    =>
        Program.Elements.Object_Renaming_Declarations.Object_Subtype);

   function F28_3 is new Generic_Child
     (Element      =>
        Program.Elements.Object_Renaming_Declarations
          .Object_Renaming_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Object_Renaming_Declarations.Renamed_Object);

   function F28_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Object_Renaming_Declarations
          .Object_Renaming_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Object_Renaming_Declarations.Aspects);

   F28  : aliased constant Getter_Array :=
     (1 => (True, Names, F28_1'Access),
      2 => (False, Object_Subtype, F28_2'Access),
      3 => (False, Renamed_Object, F28_3'Access),
      4 => (True, Aspects, F28_4'Access));

   overriding procedure Object_Renaming_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Object_Renaming_Declarations
         .Object_Renaming_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F28'Access;
   end Object_Renaming_Declaration;

   function F29_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Exception_Renaming_Declarations
          .Exception_Renaming_Declaration,
      Vector        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Vector,
      Vector_Access =>
        Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access,
      Get_Vector    => Program.Elements.Exception_Renaming_Declarations.Names);

   function F29_2 is new Generic_Child
     (Element      =>
        Program.Elements.Exception_Renaming_Declarations
          .Exception_Renaming_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Exception_Renaming_Declarations.Renamed_Exception);

   function F29_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Exception_Renaming_Declarations
          .Exception_Renaming_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Exception_Renaming_Declarations.Aspects);

   F29  : aliased constant Getter_Array :=
     (1 => (True, Names, F29_1'Access),
      2 => (False, Renamed_Exception, F29_2'Access),
      3 => (True, Aspects, F29_3'Access));

   overriding procedure Exception_Renaming_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Exception_Renaming_Declarations
         .Exception_Renaming_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F29'Access;
   end Exception_Renaming_Declaration;

   function F30_1 is new Generic_Child
     (Element      =>
        Program.Elements.Procedure_Renaming_Declarations
          .Procedure_Renaming_Declaration,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    => Program.Elements.Procedure_Renaming_Declarations.Name);

   function F30_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Procedure_Renaming_Declarations
          .Procedure_Renaming_Declaration,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Procedure_Renaming_Declarations.Parameters);

   function F30_3 is new Generic_Child
     (Element      =>
        Program.Elements.Procedure_Renaming_Declarations
          .Procedure_Renaming_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Procedure_Renaming_Declarations.Renamed_Procedure);

   function F30_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Procedure_Renaming_Declarations
          .Procedure_Renaming_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Procedure_Renaming_Declarations.Aspects);

   F30  : aliased constant Getter_Array :=
     (1 => (False, Name, F30_1'Access),
      2 => (True, Parameters, F30_2'Access),
      3 => (False, Renamed_Procedure, F30_3'Access),
      4 => (True, Aspects, F30_4'Access));

   overriding procedure Procedure_Renaming_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Procedure_Renaming_Declarations
         .Procedure_Renaming_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F30'Access;
   end Procedure_Renaming_Declaration;

   function F31_1 is new Generic_Child
     (Element      =>
        Program.Elements.Function_Renaming_Declarations
          .Function_Renaming_Declaration,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    => Program.Elements.Function_Renaming_Declarations.Name);

   function F31_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Function_Renaming_Declarations
          .Function_Renaming_Declaration,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Function_Renaming_Declarations.Parameters);

   function F31_3 is new Generic_Child
     (Element      =>
        Program.Elements.Function_Renaming_Declarations
          .Function_Renaming_Declaration,
      Child        => Program.Elements.Element,
      Child_Access => Program.Elements.Element_Access,
      Get_Child    =>
        Program.Elements.Function_Renaming_Declarations.Result_Subtype);

   function F31_4 is new Generic_Child
     (Element      =>
        Program.Elements.Function_Renaming_Declarations
          .Function_Renaming_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Function_Renaming_Declarations.Renamed_Function);

   function F31_5 is new Generic_Vector
     (Parent        =>
        Program.Elements.Function_Renaming_Declarations
          .Function_Renaming_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Function_Renaming_Declarations.Aspects);

   F31  : aliased constant Getter_Array :=
     (1 => (False, Name, F31_1'Access),
      2 => (True, Parameters, F31_2'Access),
      3 => (False, Result_Subtype, F31_3'Access),
      4 => (False, Renamed_Function, F31_4'Access),
      5 => (True, Aspects, F31_5'Access));

   overriding procedure Function_Renaming_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Function_Renaming_Declarations
         .Function_Renaming_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F31'Access;
   end Function_Renaming_Declaration;

   function F32_1 is new Generic_Child
     (Element      =>
        Program.Elements.Package_Renaming_Declarations
          .Package_Renaming_Declaration,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    => Program.Elements.Package_Renaming_Declarations.Name);

   function F32_2 is new Generic_Child
     (Element      =>
        Program.Elements.Package_Renaming_Declarations
          .Package_Renaming_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Package_Renaming_Declarations.Renamed_Package);

   function F32_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Package_Renaming_Declarations
          .Package_Renaming_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Package_Renaming_Declarations.Aspects);

   F32  : aliased constant Getter_Array :=
     (1 => (False, Name, F32_1'Access),
      2 => (False, Renamed_Package, F32_2'Access),
      3 => (True, Aspects, F32_3'Access));

   overriding procedure Package_Renaming_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Package_Renaming_Declarations
         .Package_Renaming_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F32'Access;
   end Package_Renaming_Declaration;

   function F33_1 is new Generic_Child
     (Element      =>
        Program.Elements.Generic_Package_Renaming_Declarations
          .Generic_Package_Renaming_Declaration,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    =>
        Program.Elements.Generic_Package_Renaming_Declarations.Name);

   function F33_2 is new Generic_Child
     (Element      =>
        Program.Elements.Generic_Package_Renaming_Declarations
          .Generic_Package_Renaming_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Generic_Package_Renaming_Declarations
          .Renamed_Package);

   function F33_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Generic_Package_Renaming_Declarations
          .Generic_Package_Renaming_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Generic_Package_Renaming_Declarations.Aspects);

   F33  : aliased constant Getter_Array :=
     (1 => (False, Name, F33_1'Access),
      2 => (False, Renamed_Package, F33_2'Access),
      3 => (True, Aspects, F33_3'Access));

   overriding procedure Generic_Package_Renaming_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Generic_Package_Renaming_Declarations
         .Generic_Package_Renaming_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F33'Access;
   end Generic_Package_Renaming_Declaration;

   function F34_1 is new Generic_Child
     (Element      =>
        Program.Elements.Generic_Procedure_Renaming_Declarations
          .Generic_Procedure_Renaming_Declaration,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    =>
        Program.Elements.Generic_Procedure_Renaming_Declarations.Name);

   function F34_2 is new Generic_Child
     (Element      =>
        Program.Elements.Generic_Procedure_Renaming_Declarations
          .Generic_Procedure_Renaming_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Generic_Procedure_Renaming_Declarations
          .Renamed_Procedure);

   function F34_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Generic_Procedure_Renaming_Declarations
          .Generic_Procedure_Renaming_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Generic_Procedure_Renaming_Declarations.Aspects);

   F34  : aliased constant Getter_Array :=
     (1 => (False, Name, F34_1'Access),
      2 => (False, Renamed_Procedure, F34_2'Access),
      3 => (True, Aspects, F34_3'Access));

   overriding procedure Generic_Procedure_Renaming_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements
         .Generic_Procedure_Renaming_Declarations
         .Generic_Procedure_Renaming_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F34'Access;
   end Generic_Procedure_Renaming_Declaration;

   function F35_1 is new Generic_Child
     (Element      =>
        Program.Elements.Generic_Function_Renaming_Declarations
          .Generic_Function_Renaming_Declaration,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    =>
        Program.Elements.Generic_Function_Renaming_Declarations.Name);

   function F35_2 is new Generic_Child
     (Element      =>
        Program.Elements.Generic_Function_Renaming_Declarations
          .Generic_Function_Renaming_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Generic_Function_Renaming_Declarations
          .Renamed_Function);

   function F35_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Generic_Function_Renaming_Declarations
          .Generic_Function_Renaming_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Generic_Function_Renaming_Declarations.Aspects);

   F35  : aliased constant Getter_Array :=
     (1 => (False, Name, F35_1'Access),
      2 => (False, Renamed_Function, F35_2'Access),
      3 => (True, Aspects, F35_3'Access));

   overriding procedure Generic_Function_Renaming_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Generic_Function_Renaming_Declarations
         .Generic_Function_Renaming_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F35'Access;
   end Generic_Function_Renaming_Declaration;

   function F36_1 is new Generic_Child
     (Element      =>
        Program.Elements.Task_Body_Declarations.Task_Body_Declaration,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Task_Body_Declarations.Name);

   function F36_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Task_Body_Declarations.Task_Body_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Task_Body_Declarations.Aspects);

   function F36_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Task_Body_Declarations.Task_Body_Declaration,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Task_Body_Declarations.Declarations);

   function F36_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Task_Body_Declarations.Task_Body_Declaration,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Task_Body_Declarations.Statements);

   function F36_5 is new Generic_Vector
     (Parent        =>
        Program.Elements.Task_Body_Declarations.Task_Body_Declaration,
      Vector        =>
        Program.Elements.Exception_Handlers.Exception_Handler_Vector,
      Vector_Access =>
        Program.Elements.Exception_Handlers.Exception_Handler_Vector_Access,
      Get_Vector    =>
        Program.Elements.Task_Body_Declarations.Exception_Handlers);

   function F36_6 is new Generic_Child
     (Element      =>
        Program.Elements.Task_Body_Declarations.Task_Body_Declaration,
      Child        => Program.Elements.Identifiers.Identifier,
      Child_Access => Program.Elements.Identifiers.Identifier_Access,
      Get_Child    => Program.Elements.Task_Body_Declarations.End_Name);

   F36  : aliased constant Getter_Array :=
     (1 => (False, Name, F36_1'Access),
      2 => (True, Aspects, F36_2'Access),
      3 => (True, Declarations, F36_3'Access),
      4 => (True, Statements, F36_4'Access),
      5 => (True, Exception_Handlers, F36_5'Access),
      6 => (False, End_Name, F36_6'Access));

   overriding procedure Task_Body_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Task_Body_Declarations
         .Task_Body_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F36'Access;
   end Task_Body_Declaration;

   function F37_1 is new Generic_Child
     (Element      =>
        Program.Elements.Protected_Body_Declarations
          .Protected_Body_Declaration,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Protected_Body_Declarations.Name);

   function F37_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Protected_Body_Declarations
          .Protected_Body_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Protected_Body_Declarations.Aspects);

   function F37_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Protected_Body_Declarations
          .Protected_Body_Declaration,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    =>
        Program.Elements.Protected_Body_Declarations.Protected_Operations);

   function F37_4 is new Generic_Child
     (Element      =>
        Program.Elements.Protected_Body_Declarations
          .Protected_Body_Declaration,
      Child        => Program.Elements.Identifiers.Identifier,
      Child_Access => Program.Elements.Identifiers.Identifier_Access,
      Get_Child    => Program.Elements.Protected_Body_Declarations.End_Name);

   F37  : aliased constant Getter_Array :=
     (1 => (False, Name, F37_1'Access),
      2 => (True, Aspects, F37_2'Access),
      3 => (True, Protected_Operations, F37_3'Access),
      4 => (False, End_Name, F37_4'Access));

   overriding procedure Protected_Body_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Protected_Body_Declarations
         .Protected_Body_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F37'Access;
   end Protected_Body_Declaration;

   function F38_1 is new Generic_Child
     (Element      => Program.Elements.Entry_Declarations.Entry_Declaration,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Entry_Declarations.Name);

   function F38_2 is new Generic_Child
     (Element      => Program.Elements.Entry_Declarations.Entry_Declaration,
      Child        => Program.Elements.Discrete_Ranges.Discrete_Range,
      Child_Access => Program.Elements.Discrete_Ranges.Discrete_Range_Access,
      Get_Child    =>
        Program.Elements.Entry_Declarations.Entry_Family_Definition);

   function F38_3 is new Generic_Vector
     (Parent        => Program.Elements.Entry_Declarations.Entry_Declaration,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Entry_Declarations.Parameters);

   function F38_4 is new Generic_Vector
     (Parent        => Program.Elements.Entry_Declarations.Entry_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Entry_Declarations.Aspects);

   F38  : aliased constant Getter_Array :=
     (1 => (False, Name, F38_1'Access),
      2 =>
        (False,
         Entry_Family_Definition,
         F38_2'Access),
      3 => (True, Parameters, F38_3'Access),
      4 => (True, Aspects, F38_4'Access));

   overriding procedure Entry_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Entry_Declarations
         .Entry_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F38'Access;
   end Entry_Declaration;

   function F39_1 is new Generic_Child
     (Element      =>
        Program.Elements.Entry_Body_Declarations.Entry_Body_Declaration,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Entry_Body_Declarations.Name);

   function F39_2 is new Generic_Child
     (Element      =>
        Program.Elements.Entry_Body_Declarations.Entry_Body_Declaration,
      Child        =>
        Program.Elements.Entry_Index_Specifications.Entry_Index_Specification,
      Child_Access =>
        Program.Elements.Entry_Index_Specifications
          .Entry_Index_Specification_Access,
      Get_Child    => Program.Elements.Entry_Body_Declarations.Entry_Index);

   function F39_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Entry_Body_Declarations.Entry_Body_Declaration,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Entry_Body_Declarations.Parameters);

   function F39_4 is new Generic_Child
     (Element      =>
        Program.Elements.Entry_Body_Declarations.Entry_Body_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Entry_Body_Declarations.Entry_Barrier);

   function F39_5 is new Generic_Vector
     (Parent        =>
        Program.Elements.Entry_Body_Declarations.Entry_Body_Declaration,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Entry_Body_Declarations.Declarations);

   function F39_6 is new Generic_Vector
     (Parent        =>
        Program.Elements.Entry_Body_Declarations.Entry_Body_Declaration,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Entry_Body_Declarations.Statements);

   function F39_7 is new Generic_Vector
     (Parent        =>
        Program.Elements.Entry_Body_Declarations.Entry_Body_Declaration,
      Vector        =>
        Program.Elements.Exception_Handlers.Exception_Handler_Vector,
      Vector_Access =>
        Program.Elements.Exception_Handlers.Exception_Handler_Vector_Access,
      Get_Vector    =>
        Program.Elements.Entry_Body_Declarations.Exception_Handlers);

   function F39_8 is new Generic_Child
     (Element      =>
        Program.Elements.Entry_Body_Declarations.Entry_Body_Declaration,
      Child        => Program.Elements.Identifiers.Identifier,
      Child_Access => Program.Elements.Identifiers.Identifier_Access,
      Get_Child    => Program.Elements.Entry_Body_Declarations.End_Name);

   F39  : aliased constant Getter_Array :=
     (1 => (False, Name, F39_1'Access),
      2 => (False, Entry_Index, F39_2'Access),
      3 => (True, Parameters, F39_3'Access),
      4 => (False, Entry_Barrier, F39_4'Access),
      5 => (True, Declarations, F39_5'Access),
      6 => (True, Statements, F39_6'Access),
      7 => (True, Exception_Handlers, F39_7'Access),
      8 => (False, End_Name, F39_8'Access));

   overriding procedure Entry_Body_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Entry_Body_Declarations
         .Entry_Body_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F39'Access;
   end Entry_Body_Declaration;

   function F40_1 is new Generic_Child
     (Element      =>
        Program.Elements.Entry_Index_Specifications.Entry_Index_Specification,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Entry_Index_Specifications.Name);

   function F40_2 is new Generic_Child
     (Element      =>
        Program.Elements.Entry_Index_Specifications.Entry_Index_Specification,
      Child        => Program.Elements.Discrete_Ranges.Discrete_Range,
      Child_Access => Program.Elements.Discrete_Ranges.Discrete_Range_Access,
      Get_Child    =>
        Program.Elements.Entry_Index_Specifications.Entry_Index_Subtype);

   F40  : aliased constant Getter_Array :=
     (1 => (False, Name, F40_1'Access),
      2 => (False, Entry_Index_Subtype, F40_2'Access));

   overriding procedure Entry_Index_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Entry_Index_Specifications
         .Entry_Index_Specification_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F40'Access;
   end Entry_Index_Specification;

   function F41_1 is new Generic_Child
     (Element      =>
        Program.Elements.Procedure_Body_Stubs.Procedure_Body_Stub,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Procedure_Body_Stubs.Name);

   function F41_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Procedure_Body_Stubs.Procedure_Body_Stub,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Procedure_Body_Stubs.Parameters);

   function F41_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Procedure_Body_Stubs.Procedure_Body_Stub,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Procedure_Body_Stubs.Aspects);

   F41  : aliased constant Getter_Array :=
     (1 => (False, Name, F41_1'Access),
      2 => (True, Parameters, F41_2'Access),
      3 => (True, Aspects, F41_3'Access));

   overriding procedure Procedure_Body_Stub
    (Self    : in out Visitor;
     Element : not null Program.Elements.Procedure_Body_Stubs
         .Procedure_Body_Stub_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F41'Access;
   end Procedure_Body_Stub;

   function F42_1 is new Generic_Child
     (Element      => Program.Elements.Function_Body_Stubs.Function_Body_Stub,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Function_Body_Stubs.Name);

   function F42_2 is new Generic_Vector
     (Parent        => Program.Elements.Function_Body_Stubs.Function_Body_Stub,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Function_Body_Stubs.Parameters);

   function F42_3 is new Generic_Child
     (Element      => Program.Elements.Function_Body_Stubs.Function_Body_Stub,
      Child        => Program.Elements.Element,
      Child_Access => Program.Elements.Element_Access,
      Get_Child    => Program.Elements.Function_Body_Stubs.Result_Subtype);

   function F42_4 is new Generic_Vector
     (Parent        => Program.Elements.Function_Body_Stubs.Function_Body_Stub,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Function_Body_Stubs.Aspects);

   F42  : aliased constant Getter_Array :=
     (1 => (False, Name, F42_1'Access),
      2 => (True, Parameters, F42_2'Access),
      3 => (False, Result_Subtype, F42_3'Access),
      4 => (True, Aspects, F42_4'Access));

   overriding procedure Function_Body_Stub
    (Self    : in out Visitor;
     Element : not null Program.Elements.Function_Body_Stubs
         .Function_Body_Stub_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F42'Access;
   end Function_Body_Stub;

   function F43_1 is new Generic_Child
     (Element      => Program.Elements.Package_Body_Stubs.Package_Body_Stub,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Package_Body_Stubs.Name);

   function F43_2 is new Generic_Vector
     (Parent        => Program.Elements.Package_Body_Stubs.Package_Body_Stub,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Package_Body_Stubs.Aspects);

   F43  : aliased constant Getter_Array :=
     (1 => (False, Name, F43_1'Access),
      2 => (True, Aspects, F43_2'Access));

   overriding procedure Package_Body_Stub
    (Self    : in out Visitor;
     Element : not null Program.Elements.Package_Body_Stubs
         .Package_Body_Stub_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F43'Access;
   end Package_Body_Stub;

   function F44_1 is new Generic_Child
     (Element      => Program.Elements.Task_Body_Stubs.Task_Body_Stub,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Task_Body_Stubs.Name);

   function F44_2 is new Generic_Vector
     (Parent        => Program.Elements.Task_Body_Stubs.Task_Body_Stub,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Task_Body_Stubs.Aspects);

   F44  : aliased constant Getter_Array :=
     (1 => (False, Name, F44_1'Access),
      2 => (True, Aspects, F44_2'Access));

   overriding procedure Task_Body_Stub
    (Self    : in out Visitor;
     Element : not null Program.Elements.Task_Body_Stubs
         .Task_Body_Stub_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F44'Access;
   end Task_Body_Stub;

   function F45_1 is new Generic_Child
     (Element      =>
        Program.Elements.Protected_Body_Stubs.Protected_Body_Stub,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Protected_Body_Stubs.Name);

   function F45_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Protected_Body_Stubs.Protected_Body_Stub,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Protected_Body_Stubs.Aspects);

   F45  : aliased constant Getter_Array :=
     (1 => (False, Name, F45_1'Access),
      2 => (True, Aspects, F45_2'Access));

   overriding procedure Protected_Body_Stub
    (Self    : in out Visitor;
     Element : not null Program.Elements.Protected_Body_Stubs
         .Protected_Body_Stub_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F45'Access;
   end Protected_Body_Stub;

   function F46_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Exception_Declarations.Exception_Declaration,
      Vector        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Vector,
      Vector_Access =>
        Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access,
      Get_Vector    => Program.Elements.Exception_Declarations.Names);

   function F46_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Exception_Declarations.Exception_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Exception_Declarations.Aspects);

   F46  : aliased constant Getter_Array :=
     (1 => (True, Names, F46_1'Access),
      2 => (True, Aspects, F46_2'Access));

   overriding procedure Exception_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Exception_Declarations
         .Exception_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F46'Access;
   end Exception_Declaration;

   function F47_1 is new Generic_Child
     (Element      =>
        Program.Elements.Choice_Parameter_Specifications
          .Choice_Parameter_Specification,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Choice_Parameter_Specifications.Name);

   F47  : aliased constant Getter_Array :=
     (1 => (False, Name, F47_1'Access));

   overriding procedure Choice_Parameter_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Choice_Parameter_Specifications
         .Choice_Parameter_Specification_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F47'Access;
   end Choice_Parameter_Specification;

   function F48_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Generic_Package_Declarations
          .Generic_Package_Declaration,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    =>
        Program.Elements.Generic_Package_Declarations.Formal_Parameters);

   function F48_2 is new Generic_Child
     (Element      =>
        Program.Elements.Generic_Package_Declarations
          .Generic_Package_Declaration,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    => Program.Elements.Generic_Package_Declarations.Name);

   function F48_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Generic_Package_Declarations
          .Generic_Package_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Generic_Package_Declarations.Aspects);

   function F48_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Generic_Package_Declarations
          .Generic_Package_Declaration,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    =>
        Program.Elements.Generic_Package_Declarations.Visible_Declarations);

   function F48_5 is new Generic_Vector
     (Parent        =>
        Program.Elements.Generic_Package_Declarations
          .Generic_Package_Declaration,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    =>
        Program.Elements.Generic_Package_Declarations.Private_Declarations);

   function F48_6 is new Generic_Child
     (Element      =>
        Program.Elements.Generic_Package_Declarations
          .Generic_Package_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Generic_Package_Declarations.End_Name);

   F48  : aliased constant Getter_Array :=
     (1 => (True, Formal_Parameters, F48_1'Access),
      2 => (False, Name, F48_2'Access),
      3 => (True, Aspects, F48_3'Access),
      4 => (True, Visible_Declarations, F48_4'Access),
      5 => (True, Private_Declarations, F48_5'Access),
      6 => (False, End_Name, F48_6'Access));

   overriding procedure Generic_Package_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Generic_Package_Declarations
         .Generic_Package_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F48'Access;
   end Generic_Package_Declaration;

   function F49_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Generic_Procedure_Declarations
          .Generic_Procedure_Declaration,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    =>
        Program.Elements.Generic_Procedure_Declarations.Formal_Parameters);

   function F49_2 is new Generic_Child
     (Element      =>
        Program.Elements.Generic_Procedure_Declarations
          .Generic_Procedure_Declaration,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    => Program.Elements.Generic_Procedure_Declarations.Name);

   function F49_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Generic_Procedure_Declarations
          .Generic_Procedure_Declaration,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Generic_Procedure_Declarations.Parameters);

   function F49_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Generic_Procedure_Declarations
          .Generic_Procedure_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Generic_Procedure_Declarations.Aspects);

   F49  : aliased constant Getter_Array :=
     (1 => (True, Formal_Parameters, F49_1'Access),
      2 => (False, Name, F49_2'Access),
      3 => (True, Parameters, F49_3'Access),
      4 => (True, Aspects, F49_4'Access));

   overriding procedure Generic_Procedure_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Generic_Procedure_Declarations
         .Generic_Procedure_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F49'Access;
   end Generic_Procedure_Declaration;

   function F50_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Generic_Function_Declarations
          .Generic_Function_Declaration,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    =>
        Program.Elements.Generic_Function_Declarations.Formal_Parameters);

   function F50_2 is new Generic_Child
     (Element      =>
        Program.Elements.Generic_Function_Declarations
          .Generic_Function_Declaration,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    => Program.Elements.Generic_Function_Declarations.Name);

   function F50_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Generic_Function_Declarations
          .Generic_Function_Declaration,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Generic_Function_Declarations.Parameters);

   function F50_4 is new Generic_Child
     (Element      =>
        Program.Elements.Generic_Function_Declarations
          .Generic_Function_Declaration,
      Child        => Program.Elements.Element,
      Child_Access => Program.Elements.Element_Access,
      Get_Child    =>
        Program.Elements.Generic_Function_Declarations.Result_Subtype);

   function F50_5 is new Generic_Vector
     (Parent        =>
        Program.Elements.Generic_Function_Declarations
          .Generic_Function_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Generic_Function_Declarations.Aspects);

   F50  : aliased constant Getter_Array :=
     (1 => (True, Formal_Parameters, F50_1'Access),
      2 => (False, Name, F50_2'Access),
      3 => (True, Parameters, F50_3'Access),
      4 => (False, Result_Subtype, F50_4'Access),
      5 => (True, Aspects, F50_5'Access));

   overriding procedure Generic_Function_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Generic_Function_Declarations
         .Generic_Function_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F50'Access;
   end Generic_Function_Declaration;

   function F51_1 is new Generic_Child
     (Element      =>
        Program.Elements.Package_Instantiations.Package_Instantiation,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    => Program.Elements.Package_Instantiations.Name);

   function F51_2 is new Generic_Child
     (Element      =>
        Program.Elements.Package_Instantiations.Package_Instantiation,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Package_Instantiations.Generic_Package_Name);

   function F51_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Package_Instantiations.Package_Instantiation,
      Vector        =>
        Program.Elements.Parameter_Associations.Parameter_Association_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access,
      Get_Vector    => Program.Elements.Package_Instantiations.Parameters);

   function F51_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Package_Instantiations.Package_Instantiation,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Package_Instantiations.Aspects);

   F51  : aliased constant Getter_Array :=
     (1 => (False, Name, F51_1'Access),
      2 => (False, Generic_Package_Name, F51_2'Access),
      3 => (True, Parameters, F51_3'Access),
      4 => (True, Aspects, F51_4'Access));

   overriding procedure Package_Instantiation
    (Self    : in out Visitor;
     Element : not null Program.Elements.Package_Instantiations
         .Package_Instantiation_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F51'Access;
   end Package_Instantiation;

   function F52_1 is new Generic_Child
     (Element      =>
        Program.Elements.Procedure_Instantiations.Procedure_Instantiation,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    => Program.Elements.Procedure_Instantiations.Name);

   function F52_2 is new Generic_Child
     (Element      =>
        Program.Elements.Procedure_Instantiations.Procedure_Instantiation,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Procedure_Instantiations.Generic_Procedure_Name);

   function F52_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Procedure_Instantiations.Procedure_Instantiation,
      Vector        =>
        Program.Elements.Parameter_Associations.Parameter_Association_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access,
      Get_Vector    => Program.Elements.Procedure_Instantiations.Parameters);

   function F52_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Procedure_Instantiations.Procedure_Instantiation,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Procedure_Instantiations.Aspects);

   F52  : aliased constant Getter_Array :=
     (1 => (False, Name, F52_1'Access),
      2 =>
        (False,
         Generic_Procedure_Name,
         F52_2'Access),
      3 => (True, Parameters, F52_3'Access),
      4 => (True, Aspects, F52_4'Access));

   overriding procedure Procedure_Instantiation
    (Self    : in out Visitor;
     Element : not null Program.Elements.Procedure_Instantiations
         .Procedure_Instantiation_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F52'Access;
   end Procedure_Instantiation;

   function F53_1 is new Generic_Child
     (Element      =>
        Program.Elements.Function_Instantiations.Function_Instantiation,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    => Program.Elements.Function_Instantiations.Name);

   function F53_2 is new Generic_Child
     (Element      =>
        Program.Elements.Function_Instantiations.Function_Instantiation,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Function_Instantiations.Generic_Function_Name);

   function F53_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Function_Instantiations.Function_Instantiation,
      Vector        =>
        Program.Elements.Parameter_Associations.Parameter_Association_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access,
      Get_Vector    => Program.Elements.Function_Instantiations.Parameters);

   function F53_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Function_Instantiations.Function_Instantiation,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Function_Instantiations.Aspects);

   F53  : aliased constant Getter_Array :=
     (1 => (False, Name, F53_1'Access),
      2 =>
        (False, Generic_Function_Name, F53_2'Access),
      3 => (True, Parameters, F53_3'Access),
      4 => (True, Aspects, F53_4'Access));

   overriding procedure Function_Instantiation
    (Self    : in out Visitor;
     Element : not null Program.Elements.Function_Instantiations
         .Function_Instantiation_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F53'Access;
   end Function_Instantiation;

   function F54_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Formal_Object_Declarations.Formal_Object_Declaration,
      Vector        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Vector,
      Vector_Access =>
        Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access,
      Get_Vector    => Program.Elements.Formal_Object_Declarations.Names);

   function F54_2 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Object_Declarations.Formal_Object_Declaration,
      Child        => Program.Elements.Element,
      Child_Access => Program.Elements.Element_Access,
      Get_Child    =>
        Program.Elements.Formal_Object_Declarations.Object_Subtype);

   function F54_3 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Object_Declarations.Formal_Object_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Formal_Object_Declarations.Default_Expression);

   function F54_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Formal_Object_Declarations.Formal_Object_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Formal_Object_Declarations.Aspects);

   F54  : aliased constant Getter_Array :=
     (1 => (True, Names, F54_1'Access),
      2 => (False, Object_Subtype, F54_2'Access),
      3 => (False, Default_Expression, F54_3'Access),
      4 => (True, Aspects, F54_4'Access));

   overriding procedure Formal_Object_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Object_Declarations
         .Formal_Object_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F54'Access;
   end Formal_Object_Declaration;

   function F55_1 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Type_Declarations.Formal_Type_Declaration,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Formal_Type_Declarations.Name);

   function F55_2 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Type_Declarations.Formal_Type_Declaration,
      Child        => Program.Elements.Definitions.Definition,
      Child_Access => Program.Elements.Definitions.Definition_Access,
      Get_Child    =>
        Program.Elements.Formal_Type_Declarations.Discriminant_Part);

   function F55_3 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Type_Declarations.Formal_Type_Declaration,
      Child        =>
        Program.Elements.Formal_Type_Definitions.Formal_Type_Definition,
      Child_Access =>
        Program.Elements.Formal_Type_Definitions.Formal_Type_Definition_Access,
      Get_Child    => Program.Elements.Formal_Type_Declarations.Definition);

   function F55_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Formal_Type_Declarations.Formal_Type_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Formal_Type_Declarations.Aspects);

   F55  : aliased constant Getter_Array :=
     (1 => (False, Name, F55_1'Access),
      2 => (False, Discriminant_Part, F55_2'Access),
      3 => (False, Definition, F55_3'Access),
      4 => (True, Aspects, F55_4'Access));

   overriding procedure Formal_Type_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Type_Declarations
         .Formal_Type_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F55'Access;
   end Formal_Type_Declaration;

   function F56_1 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Procedure_Declarations
          .Formal_Procedure_Declaration,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Formal_Procedure_Declarations.Name);

   function F56_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Formal_Procedure_Declarations
          .Formal_Procedure_Declaration,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Formal_Procedure_Declarations.Parameters);

   function F56_3 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Procedure_Declarations
          .Formal_Procedure_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Formal_Procedure_Declarations.Subprogram_Default);

   function F56_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Formal_Procedure_Declarations
          .Formal_Procedure_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Formal_Procedure_Declarations.Aspects);

   F56  : aliased constant Getter_Array :=
     (1 => (False, Name, F56_1'Access),
      2 => (True, Parameters, F56_2'Access),
      3 => (False, Subprogram_Default, F56_3'Access),
      4 => (True, Aspects, F56_4'Access));

   overriding procedure Formal_Procedure_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Procedure_Declarations
         .Formal_Procedure_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F56'Access;
   end Formal_Procedure_Declaration;

   function F57_1 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Function_Declarations
          .Formal_Function_Declaration,
      Child        => Program.Elements.Defining_Names.Defining_Name,
      Child_Access => Program.Elements.Defining_Names.Defining_Name_Access,
      Get_Child    => Program.Elements.Formal_Function_Declarations.Name);

   function F57_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Formal_Function_Declarations
          .Formal_Function_Declaration,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Formal_Function_Declarations.Parameters);

   function F57_3 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Function_Declarations
          .Formal_Function_Declaration,
      Child        => Program.Elements.Element,
      Child_Access => Program.Elements.Element_Access,
      Get_Child    =>
        Program.Elements.Formal_Function_Declarations.Result_Subtype);

   function F57_4 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Function_Declarations
          .Formal_Function_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Formal_Function_Declarations.Subprogram_Default);

   function F57_5 is new Generic_Vector
     (Parent        =>
        Program.Elements.Formal_Function_Declarations
          .Formal_Function_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Formal_Function_Declarations.Aspects);

   F57  : aliased constant Getter_Array :=
     (1 => (False, Name, F57_1'Access),
      2 => (True, Parameters, F57_2'Access),
      3 => (False, Result_Subtype, F57_3'Access),
      4 => (False, Subprogram_Default, F57_4'Access),
      5 => (True, Aspects, F57_5'Access));

   overriding procedure Formal_Function_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Function_Declarations
         .Formal_Function_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F57'Access;
   end Formal_Function_Declaration;

   function F58_1 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Package_Declarations
          .Formal_Package_Declaration,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Formal_Package_Declarations.Name);

   function F58_2 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Package_Declarations
          .Formal_Package_Declaration,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Formal_Package_Declarations.Generic_Package_Name);

   function F58_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Formal_Package_Declarations
          .Formal_Package_Declaration,
      Vector        =>
        Program.Elements.Formal_Package_Associations
          .Formal_Package_Association_Vector,
      Vector_Access =>
        Program.Elements.Formal_Package_Associations
          .Formal_Package_Association_Vector_Access,
      Get_Vector    =>
        Program.Elements.Formal_Package_Declarations.Parameters);

   function F58_4 is new Generic_Vector
     (Parent        =>
        Program.Elements.Formal_Package_Declarations
          .Formal_Package_Declaration,
      Vector        =>
        Program.Elements.Aspect_Specifications.Aspect_Specification_Vector,
      Vector_Access =>
        Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Formal_Package_Declarations.Aspects);

   F58  : aliased constant Getter_Array :=
     (1 => (False, Name, F58_1'Access),
      2 => (False, Generic_Package_Name, F58_2'Access),
      3 => (True, Parameters, F58_3'Access),
      4 => (True, Aspects, F58_4'Access));

   overriding procedure Formal_Package_Declaration
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Package_Declarations
         .Formal_Package_Declaration_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F58'Access;
   end Formal_Package_Declaration;

   function F59_1 is new Generic_Child
     (Element      => Program.Elements.Subtype_Indications.Subtype_Indication,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Subtype_Indications.Subtype_Mark);

   function F59_2 is new Generic_Child
     (Element      => Program.Elements.Subtype_Indications.Subtype_Indication,
      Child        => Program.Elements.Constraints.Constraint,
      Child_Access => Program.Elements.Constraints.Constraint_Access,
      Get_Child    => Program.Elements.Subtype_Indications.Constraint);

   F59  : aliased constant Getter_Array :=
     (1 => (False, Subtype_Mark, F59_1'Access),
      2 => (False, Constraint, F59_2'Access));

   overriding procedure Subtype_Indication
    (Self    : in out Visitor;
     Element : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F59'Access;
   end Subtype_Indication;

   function F60_1 is new Generic_Child
     (Element      =>
        Program.Elements.Component_Definitions.Component_Definition,
      Child        => Program.Elements.Element,
      Child_Access => Program.Elements.Element_Access,
      Get_Child    =>
        Program.Elements.Component_Definitions.Subtype_Indication);

   F60  : aliased constant Getter_Array :=
     (1 => (False, Subtype_Indication, F60_1'Access));

   overriding procedure Component_Definition
    (Self    : in out Visitor;
     Element : not null Program.Elements.Component_Definitions
         .Component_Definition_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F60'Access;
   end Component_Definition;

   function F61_1 is new Generic_Child
     (Element      =>
        Program.Elements.Discrete_Subtype_Indications
          .Discrete_Subtype_Indication,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Discrete_Subtype_Indications.Subtype_Mark);

   function F61_2 is new Generic_Child
     (Element      =>
        Program.Elements.Discrete_Subtype_Indications
          .Discrete_Subtype_Indication,
      Child        => Program.Elements.Constraints.Constraint,
      Child_Access => Program.Elements.Constraints.Constraint_Access,
      Get_Child    =>
        Program.Elements.Discrete_Subtype_Indications.Constraint);

   F61  : aliased constant Getter_Array :=
     (1 => (False, Subtype_Mark, F61_1'Access),
      2 => (False, Constraint, F61_2'Access));

   overriding procedure Discrete_Subtype_Indication
    (Self    : in out Visitor;
     Element : not null Program.Elements.Discrete_Subtype_Indications
         .Discrete_Subtype_Indication_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F61'Access;
   end Discrete_Subtype_Indication;

   function F62_1 is new Generic_Child
     (Element      =>
        Program.Elements.Discrete_Range_Attribute_References
          .Discrete_Range_Attribute_Reference,
      Child        =>
        Program.Elements.Attribute_References.Attribute_Reference,
      Child_Access =>
        Program.Elements.Attribute_References.Attribute_Reference_Access,
      Get_Child    =>
        Program.Elements.Discrete_Range_Attribute_References.Range_Attribute);

   F62  : aliased constant Getter_Array :=
     (1 => (False, Range_Attribute, F62_1'Access));

   overriding procedure Discrete_Range_Attribute_Reference
    (Self    : in out Visitor;
     Element : not null Program.Elements.Discrete_Range_Attribute_References
         .Discrete_Range_Attribute_Reference_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F62'Access;
   end Discrete_Range_Attribute_Reference;

   function F63_1 is new Generic_Child
     (Element      =>
        Program.Elements.Discrete_Simple_Expression_Ranges
          .Discrete_Simple_Expression_Range,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Discrete_Simple_Expression_Ranges.Lower_Bound);

   function F63_2 is new Generic_Child
     (Element      =>
        Program.Elements.Discrete_Simple_Expression_Ranges
          .Discrete_Simple_Expression_Range,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Discrete_Simple_Expression_Ranges.Upper_Bound);

   F63  : aliased constant Getter_Array :=
     (1 => (False, Lower_Bound, F63_1'Access),
      2 => (False, Upper_Bound, F63_2'Access));

   overriding procedure Discrete_Simple_Expression_Range
    (Self    : in out Visitor;
     Element : not null Program.Elements.Discrete_Simple_Expression_Ranges
         .Discrete_Simple_Expression_Range_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F63'Access;
   end Discrete_Simple_Expression_Range;

   function F65_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Known_Discriminant_Parts.Known_Discriminant_Part,
      Vector        =>
        Program.Elements.Discriminant_Specifications
          .Discriminant_Specification_Vector,
      Vector_Access =>
        Program.Elements.Discriminant_Specifications
          .Discriminant_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Known_Discriminant_Parts.Discriminants);

   F65  : aliased constant Getter_Array :=
     (1 => (True, Discriminants, F65_1'Access));

   overriding procedure Known_Discriminant_Part
    (Self    : in out Visitor;
     Element : not null Program.Elements.Known_Discriminant_Parts
         .Known_Discriminant_Part_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F65'Access;
   end Known_Discriminant_Part;

   function F66_1 is new Generic_Vector
     (Parent        => Program.Elements.Record_Definitions.Record_Definition,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Record_Definitions.Components);

   F66  : aliased constant Getter_Array :=
     (1 => (True, Components, F66_1'Access));

   overriding procedure Record_Definition
    (Self    : in out Visitor;
     Element : not null Program.Elements.Record_Definitions
         .Record_Definition_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F66'Access;
   end Record_Definition;

   function F68_1 is new Generic_Child
     (Element      => Program.Elements.Variant_Parts.Variant_Part,
      Child        => Program.Elements.Identifiers.Identifier,
      Child_Access => Program.Elements.Identifiers.Identifier_Access,
      Get_Child    => Program.Elements.Variant_Parts.Discriminant);

   function F68_2 is new Generic_Vector
     (Parent        => Program.Elements.Variant_Parts.Variant_Part,
      Vector        => Program.Elements.Variants.Variant_Vector,
      Vector_Access => Program.Elements.Variants.Variant_Vector_Access,
      Get_Vector    => Program.Elements.Variant_Parts.Variants);

   F68  : aliased constant Getter_Array :=
     (1 => (False, Discriminant, F68_1'Access),
      2 => (True, Variants, F68_2'Access));

   overriding procedure Variant_Part
    (Self    : in out Visitor;
     Element : not null Program.Elements.Variant_Parts.Variant_Part_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F68'Access;
   end Variant_Part;

   function F69_1 is new Generic_Vector
     (Parent        => Program.Elements.Variants.Variant,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Variants.Choices);

   function F69_2 is new Generic_Vector
     (Parent        => Program.Elements.Variants.Variant,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Variants.Components);

   F69  : aliased constant Getter_Array :=
     (1 => (True, Choices, F69_1'Access),
      2 => (True, Components, F69_2'Access));

   overriding procedure Variant
    (Self    : in out Visitor;
     Element : not null Program.Elements.Variants.Variant_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F69'Access;
   end Variant;

   function F71_1 is new Generic_Child
     (Element      =>
        Program.Elements.Anonymous_Access_To_Objects
          .Anonymous_Access_To_Object,
      Child        => Program.Elements.Subtype_Indications.Subtype_Indication,
      Child_Access =>
        Program.Elements.Subtype_Indications.Subtype_Indication_Access,
      Get_Child    =>
        Program.Elements.Anonymous_Access_To_Objects.Subtype_Indication);

   F71  : aliased constant Getter_Array :=
     (1 => (False, Subtype_Indication, F71_1'Access));

   overriding procedure Anonymous_Access_To_Object
    (Self    : in out Visitor;
     Element : not null Program.Elements.Anonymous_Access_To_Objects
         .Anonymous_Access_To_Object_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F71'Access;
   end Anonymous_Access_To_Object;

   function F72_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Anonymous_Access_To_Procedures
          .Anonymous_Access_To_Procedure,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Anonymous_Access_To_Procedures.Parameters);

   F72  : aliased constant Getter_Array :=
     (1 => (True, Parameters, F72_1'Access));

   overriding procedure Anonymous_Access_To_Procedure
    (Self    : in out Visitor;
     Element : not null Program.Elements.Anonymous_Access_To_Procedures
         .Anonymous_Access_To_Procedure_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F72'Access;
   end Anonymous_Access_To_Procedure;

   function F73_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Anonymous_Access_To_Functions
          .Anonymous_Access_To_Function,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Anonymous_Access_To_Functions.Parameters);

   function F73_2 is new Generic_Child
     (Element      =>
        Program.Elements.Anonymous_Access_To_Functions
          .Anonymous_Access_To_Function,
      Child        => Program.Elements.Element,
      Child_Access => Program.Elements.Element_Access,
      Get_Child    =>
        Program.Elements.Anonymous_Access_To_Functions.Result_Subtype);

   F73  : aliased constant Getter_Array :=
     (1 => (True, Parameters, F73_1'Access),
      2 => (False, Result_Subtype, F73_2'Access));

   overriding procedure Anonymous_Access_To_Function
    (Self    : in out Visitor;
     Element : not null Program.Elements.Anonymous_Access_To_Functions
         .Anonymous_Access_To_Function_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F73'Access;
   end Anonymous_Access_To_Function;

   function F75_1 is new Generic_Child
     (Element      =>
        Program.Elements.Private_Extension_Definitions
          .Private_Extension_Definition,
      Child        => Program.Elements.Subtype_Indications.Subtype_Indication,
      Child_Access =>
        Program.Elements.Subtype_Indications.Subtype_Indication_Access,
      Get_Child    => Program.Elements.Private_Extension_Definitions.Ancestor);

   function F75_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Private_Extension_Definitions
          .Private_Extension_Definition,
      Vector        => Program.Elements.Expressions.Expression_Vector,
      Vector_Access => Program.Elements.Expressions.Expression_Vector_Access,
      Get_Vector    =>
        Program.Elements.Private_Extension_Definitions.Progenitors);

   F75  : aliased constant Getter_Array :=
     (1 => (False, Ancestor, F75_1'Access),
      2 => (True, Progenitors, F75_2'Access));

   overriding procedure Private_Extension_Definition
    (Self    : in out Visitor;
     Element : not null Program.Elements.Private_Extension_Definitions
         .Private_Extension_Definition_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F75'Access;
   end Private_Extension_Definition;

   function F77_1 is new Generic_Vector
     (Parent        => Program.Elements.Task_Definitions.Task_Definition,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Task_Definitions.Visible_Declarations);

   function F77_2 is new Generic_Vector
     (Parent        => Program.Elements.Task_Definitions.Task_Definition,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Task_Definitions.Private_Declarations);

   function F77_3 is new Generic_Child
     (Element      => Program.Elements.Task_Definitions.Task_Definition,
      Child        => Program.Elements.Identifiers.Identifier,
      Child_Access => Program.Elements.Identifiers.Identifier_Access,
      Get_Child    => Program.Elements.Task_Definitions.End_Name);

   F77  : aliased constant Getter_Array :=
     (1 => (True, Visible_Declarations, F77_1'Access),
      2 => (True, Private_Declarations, F77_2'Access),
      3 => (False, End_Name, F77_3'Access));

   overriding procedure Task_Definition
    (Self    : in out Visitor;
     Element : not null Program.Elements.Task_Definitions
         .Task_Definition_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F77'Access;
   end Task_Definition;

   function F78_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Protected_Definitions.Protected_Definition,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    =>
        Program.Elements.Protected_Definitions.Visible_Declarations);

   function F78_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Protected_Definitions.Protected_Definition,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    =>
        Program.Elements.Protected_Definitions.Private_Declarations);

   function F78_3 is new Generic_Child
     (Element      =>
        Program.Elements.Protected_Definitions.Protected_Definition,
      Child        => Program.Elements.Identifiers.Identifier,
      Child_Access => Program.Elements.Identifiers.Identifier_Access,
      Get_Child    => Program.Elements.Protected_Definitions.End_Name);

   F78  : aliased constant Getter_Array :=
     (1 => (True, Visible_Declarations, F78_1'Access),
      2 => (True, Private_Declarations, F78_2'Access),
      3 => (False, End_Name, F78_3'Access));

   overriding procedure Protected_Definition
    (Self    : in out Visitor;
     Element : not null Program.Elements.Protected_Definitions
         .Protected_Definition_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F78'Access;
   end Protected_Definition;

   function F79_1 is new Generic_Child
     (Element      =>
        Program.Elements.Aspect_Specifications.Aspect_Specification,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Aspect_Specifications.Aspect_Mark);

   function F79_2 is new Generic_Child
     (Element      =>
        Program.Elements.Aspect_Specifications.Aspect_Specification,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Aspect_Specifications.Aspect_Definition);

   F79  : aliased constant Getter_Array :=
     (1 => (False, Aspect_Mark, F79_1'Access),
      2 => (False, Aspect_Definition, F79_2'Access));

   overriding procedure Aspect_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F79'Access;
   end Aspect_Specification;

   function F80_1 is new Generic_Child
     (Element      =>
        Program.Elements.Real_Range_Specifications.Real_Range_Specification,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Real_Range_Specifications.Lower_Bound);

   function F80_2 is new Generic_Child
     (Element      =>
        Program.Elements.Real_Range_Specifications.Real_Range_Specification,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Real_Range_Specifications.Upper_Bound);

   F80  : aliased constant Getter_Array :=
     (1 => (False, Lower_Bound, F80_1'Access),
      2 => (False, Upper_Bound, F80_2'Access));

   overriding procedure Real_Range_Specification
    (Self    : in out Visitor;
     Element : not null Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F80'Access;
   end Real_Range_Specification;

   function F86_1 is new Generic_Child
     (Element      =>
        Program.Elements.Explicit_Dereferences.Explicit_Dereference,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Explicit_Dereferences.Prefix);

   F86  : aliased constant Getter_Array :=
     (1 => (False, Prefix, F86_1'Access));

   overriding procedure Explicit_Dereference
    (Self    : in out Visitor;
     Element : not null Program.Elements.Explicit_Dereferences
         .Explicit_Dereference_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F86'Access;
   end Explicit_Dereference;

   function F87_1 is new Generic_Child
     (Element      => Program.Elements.Function_Calls.Function_Call,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Function_Calls.Prefix);

   function F87_2 is new Generic_Vector
     (Parent        => Program.Elements.Function_Calls.Function_Call,
      Vector        =>
        Program.Elements.Parameter_Associations.Parameter_Association_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access,
      Get_Vector    => Program.Elements.Function_Calls.Parameters);

   F87  : aliased constant Getter_Array :=
     (1 => (False, Prefix, F87_1'Access),
      2 => (True, Parameters, F87_2'Access));

   overriding procedure Function_Call
    (Self    : in out Visitor;
     Element : not null Program.Elements.Function_Calls
         .Function_Call_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F87'Access;
   end Function_Call;

   function F88_1 is new Generic_Child
     (Element      => Program.Elements.Indexed_Components.Indexed_Component,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Indexed_Components.Prefix);

   function F88_2 is new Generic_Vector
     (Parent        => Program.Elements.Indexed_Components.Indexed_Component,
      Vector        => Program.Elements.Expressions.Expression_Vector,
      Vector_Access => Program.Elements.Expressions.Expression_Vector_Access,
      Get_Vector    => Program.Elements.Indexed_Components.Expressions);

   F88  : aliased constant Getter_Array :=
     (1 => (False, Prefix, F88_1'Access),
      2 => (True, Expressions, F88_2'Access));

   overriding procedure Indexed_Component
    (Self    : in out Visitor;
     Element : not null Program.Elements.Indexed_Components
         .Indexed_Component_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F88'Access;
   end Indexed_Component;

   function F89_1 is new Generic_Child
     (Element      => Program.Elements.Slices.Slice,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Slices.Prefix);

   function F89_2 is new Generic_Child
     (Element      => Program.Elements.Slices.Slice,
      Child        => Program.Elements.Discrete_Ranges.Discrete_Range,
      Child_Access => Program.Elements.Discrete_Ranges.Discrete_Range_Access,
      Get_Child    => Program.Elements.Slices.Slice_Range);

   F89  : aliased constant Getter_Array :=
     (1 => (False, Prefix, F89_1'Access),
      2 => (False, Slice_Range, F89_2'Access));

   overriding procedure Slice
    (Self    : in out Visitor;
     Element : not null Program.Elements.Slices.Slice_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F89'Access;
   end Slice;

   function F90_1 is new Generic_Child
     (Element      => Program.Elements.Selected_Components.Selected_Component,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Selected_Components.Prefix);

   function F90_2 is new Generic_Child
     (Element      => Program.Elements.Selected_Components.Selected_Component,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Selected_Components.Selector);

   F90  : aliased constant Getter_Array :=
     (1 => (False, Prefix, F90_1'Access),
      2 => (False, Selector, F90_2'Access));

   overriding procedure Selected_Component
    (Self    : in out Visitor;
     Element : not null Program.Elements.Selected_Components
         .Selected_Component_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F90'Access;
   end Selected_Component;

   function F91_1 is new Generic_Child
     (Element      =>
        Program.Elements.Attribute_References.Attribute_Reference,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Attribute_References.Prefix);

   function F91_2 is new Generic_Child
     (Element      =>
        Program.Elements.Attribute_References.Attribute_Reference,
      Child        => Program.Elements.Identifiers.Identifier,
      Child_Access => Program.Elements.Identifiers.Identifier_Access,
      Get_Child    =>
        Program.Elements.Attribute_References.Attribute_Designator);

   function F91_3 is new Generic_Child
     (Element      =>
        Program.Elements.Attribute_References.Attribute_Reference,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Attribute_References.Expressions);

   F91  : aliased constant Getter_Array :=
     (1 => (False, Prefix, F91_1'Access),
      2 => (False, Attribute_Designator, F91_2'Access),
      3 => (False, Expressions, F91_3'Access));

   overriding procedure Attribute_Reference
    (Self    : in out Visitor;
     Element : not null Program.Elements.Attribute_References
         .Attribute_Reference_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F91'Access;
   end Attribute_Reference;

   function F92_1 is new Generic_Vector
     (Parent        => Program.Elements.Record_Aggregates.Record_Aggregate,
      Vector        =>
        Program.Elements.Record_Component_Associations
          .Record_Component_Association_Vector,
      Vector_Access =>
        Program.Elements.Record_Component_Associations
          .Record_Component_Association_Vector_Access,
      Get_Vector    => Program.Elements.Record_Aggregates.Components);

   F92  : aliased constant Getter_Array :=
     (1 => (True, Components, F92_1'Access));

   overriding procedure Record_Aggregate
    (Self    : in out Visitor;
     Element : not null Program.Elements.Record_Aggregates
         .Record_Aggregate_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F92'Access;
   end Record_Aggregate;

   function F93_1 is new Generic_Child
     (Element      =>
        Program.Elements.Extension_Aggregates.Extension_Aggregate,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Extension_Aggregates.Ancestor);

   function F93_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Extension_Aggregates.Extension_Aggregate,
      Vector        =>
        Program.Elements.Record_Component_Associations
          .Record_Component_Association_Vector,
      Vector_Access =>
        Program.Elements.Record_Component_Associations
          .Record_Component_Association_Vector_Access,
      Get_Vector    => Program.Elements.Extension_Aggregates.Components);

   F93  : aliased constant Getter_Array :=
     (1 => (False, Ancestor, F93_1'Access),
      2 => (True, Components, F93_2'Access));

   overriding procedure Extension_Aggregate
    (Self    : in out Visitor;
     Element : not null Program.Elements.Extension_Aggregates
         .Extension_Aggregate_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F93'Access;
   end Extension_Aggregate;

   function F94_1 is new Generic_Vector
     (Parent        => Program.Elements.Array_Aggregates.Array_Aggregate,
      Vector        =>
        Program.Elements.Array_Component_Associations
          .Array_Component_Association_Vector,
      Vector_Access =>
        Program.Elements.Array_Component_Associations
          .Array_Component_Association_Vector_Access,
      Get_Vector    => Program.Elements.Array_Aggregates.Components);

   F94  : aliased constant Getter_Array :=
     (1 => (True, Components, F94_1'Access));

   overriding procedure Array_Aggregate
    (Self    : in out Visitor;
     Element : not null Program.Elements.Array_Aggregates
         .Array_Aggregate_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F94'Access;
   end Array_Aggregate;

   function F95_1 is new Generic_Child
     (Element      =>
        Program.Elements.Short_Circuit_Operations.Short_Circuit_Operation,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Short_Circuit_Operations.Left);

   function F95_2 is new Generic_Child
     (Element      =>
        Program.Elements.Short_Circuit_Operations.Short_Circuit_Operation,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Short_Circuit_Operations.Right);

   F95  : aliased constant Getter_Array :=
     (1 => (False, Left, F95_1'Access),
      2 => (False, Right, F95_2'Access));

   overriding procedure Short_Circuit_Operation
    (Self    : in out Visitor;
     Element : not null Program.Elements.Short_Circuit_Operations
         .Short_Circuit_Operation_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F95'Access;
   end Short_Circuit_Operation;

   function F96_1 is new Generic_Child
     (Element      => Program.Elements.Membership_Tests.Membership_Test,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Membership_Tests.Expression);

   function F96_2 is new Generic_Vector
     (Parent        => Program.Elements.Membership_Tests.Membership_Test,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Membership_Tests.Choices);

   F96  : aliased constant Getter_Array :=
     (1 => (False, Expression, F96_1'Access),
      2 => (True, Choices, F96_2'Access));

   overriding procedure Membership_Test
    (Self    : in out Visitor;
     Element : not null Program.Elements.Membership_Tests
         .Membership_Test_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F96'Access;
   end Membership_Test;

   function F98_1 is new Generic_Child
     (Element      =>
        Program.Elements.Parenthesized_Expressions.Parenthesized_Expression,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Parenthesized_Expressions.Expression);

   F98  : aliased constant Getter_Array :=
     (1 => (False, Expression, F98_1'Access));

   overriding procedure Parenthesized_Expression
    (Self    : in out Visitor;
     Element : not null Program.Elements.Parenthesized_Expressions
         .Parenthesized_Expression_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F98'Access;
   end Parenthesized_Expression;

   function F99_1 is new Generic_Child
     (Element      => Program.Elements.Raise_Expressions.Raise_Expression,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Raise_Expressions.Exception_Name);

   function F99_2 is new Generic_Child
     (Element      => Program.Elements.Raise_Expressions.Raise_Expression,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Raise_Expressions.Associated_Message);

   F99  : aliased constant Getter_Array :=
     (1 => (False, Exception_Name, F99_1'Access),
      2 => (False, Associated_Message, F99_2'Access));

   overriding procedure Raise_Expression
    (Self    : in out Visitor;
     Element : not null Program.Elements.Raise_Expressions
         .Raise_Expression_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F99'Access;
   end Raise_Expression;

   function F100_1 is new Generic_Child
     (Element      => Program.Elements.Type_Conversions.Type_Conversion,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Type_Conversions.Subtype_Mark);

   function F100_2 is new Generic_Child
     (Element      => Program.Elements.Type_Conversions.Type_Conversion,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Type_Conversions.Operand);

   F100 : aliased constant Getter_Array :=
     (1 => (False, Subtype_Mark, F100_1'Access),
      2 => (False, Operand, F100_2'Access));

   overriding procedure Type_Conversion
    (Self    : in out Visitor;
     Element : not null Program.Elements.Type_Conversions
         .Type_Conversion_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F100'Access;
   end Type_Conversion;

   function F101_1 is new Generic_Child
     (Element      =>
        Program.Elements.Qualified_Expressions.Qualified_Expression,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Qualified_Expressions.Subtype_Mark);

   function F101_2 is new Generic_Child
     (Element      =>
        Program.Elements.Qualified_Expressions.Qualified_Expression,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Qualified_Expressions.Operand);

   F101 : aliased constant Getter_Array :=
     (1 => (False, Subtype_Mark, F101_1'Access),
      2 => (False, Operand, F101_2'Access));

   overriding procedure Qualified_Expression
    (Self    : in out Visitor;
     Element : not null Program.Elements.Qualified_Expressions
         .Qualified_Expression_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F101'Access;
   end Qualified_Expression;

   function F102_1 is new Generic_Child
     (Element      => Program.Elements.Allocators.Allocator,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Allocators.Subpool_Name);

   function F102_2 is new Generic_Child
     (Element      => Program.Elements.Allocators.Allocator,
      Child        => Program.Elements.Subtype_Indications.Subtype_Indication,
      Child_Access =>
        Program.Elements.Subtype_Indications.Subtype_Indication_Access,
      Get_Child    => Program.Elements.Allocators.Subtype_Indication);

   function F102_3 is new Generic_Child
     (Element      => Program.Elements.Allocators.Allocator,
      Child        =>
        Program.Elements.Qualified_Expressions.Qualified_Expression,
      Child_Access =>
        Program.Elements.Qualified_Expressions.Qualified_Expression_Access,
      Get_Child    => Program.Elements.Allocators.Qualified_Expression);

   F102 : aliased constant Getter_Array :=
     (1 => (False, Subpool_Name, F102_1'Access),
      2 => (False, Subtype_Indication, F102_2'Access),
      3 => (False, Qualified_Expression, F102_3'Access));

   overriding procedure Allocator
    (Self    : in out Visitor;
     Element : not null Program.Elements.Allocators.Allocator_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F102'Access;
   end Allocator;

   function F103_1 is new Generic_Child
     (Element      => Program.Elements.Case_Expressions.Case_Expression,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Case_Expressions.Selecting_Expression);

   function F103_2 is new Generic_Vector
     (Parent        => Program.Elements.Case_Expressions.Case_Expression,
      Vector        =>
        Program.Elements.Case_Expression_Paths.Case_Expression_Path_Vector,
      Vector_Access =>
        Program.Elements.Case_Expression_Paths
          .Case_Expression_Path_Vector_Access,
      Get_Vector    => Program.Elements.Case_Expressions.Paths);

   F103 : aliased constant Getter_Array :=
     (1 => (False, Selecting_Expression, F103_1'Access),
      2 => (True, Paths, F103_2'Access));

   overriding procedure Case_Expression
    (Self    : in out Visitor;
     Element : not null Program.Elements.Case_Expressions
         .Case_Expression_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F103'Access;
   end Case_Expression;

   function F104_1 is new Generic_Child
     (Element      => Program.Elements.If_Expressions.If_Expression,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.If_Expressions.Condition);

   function F104_2 is new Generic_Child
     (Element      => Program.Elements.If_Expressions.If_Expression,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.If_Expressions.Then_Expression);

   function F104_3 is new Generic_Vector
     (Parent        => Program.Elements.If_Expressions.If_Expression,
      Vector        => Program.Elements.Elsif_Paths.Elsif_Path_Vector,
      Vector_Access => Program.Elements.Elsif_Paths.Elsif_Path_Vector_Access,
      Get_Vector    => Program.Elements.If_Expressions.Elsif_Paths);

   function F104_4 is new Generic_Child
     (Element      => Program.Elements.If_Expressions.If_Expression,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.If_Expressions.Else_Expression);

   F104 : aliased constant Getter_Array :=
     (1 => (False, Condition, F104_1'Access),
      2 => (False, Then_Expression, F104_2'Access),
      3 => (True, Elsif_Paths, F104_3'Access),
      4 => (False, Else_Expression, F104_4'Access));

   overriding procedure If_Expression
    (Self    : in out Visitor;
     Element : not null Program.Elements.If_Expressions
         .If_Expression_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F104'Access;
   end If_Expression;

   function F105_1 is new Generic_Child
     (Element      =>
        Program.Elements.Quantified_Expressions.Quantified_Expression,
      Child        =>
        Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification,
      Child_Access =>
        Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification_Access,
      Get_Child    => Program.Elements.Quantified_Expressions.Parameter);

   function F105_2 is new Generic_Child
     (Element      =>
        Program.Elements.Quantified_Expressions.Quantified_Expression,
      Child        =>
        Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification,
      Child_Access =>
        Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification_Access,
      Get_Child    =>
        Program.Elements.Quantified_Expressions.Generalized_Iterator);

   function F105_3 is new Generic_Child
     (Element      =>
        Program.Elements.Quantified_Expressions.Quantified_Expression,
      Child        =>
        Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification,
      Child_Access =>
        Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification_Access,
      Get_Child    =>
        Program.Elements.Quantified_Expressions.Element_Iterator);

   function F105_4 is new Generic_Child
     (Element      =>
        Program.Elements.Quantified_Expressions.Quantified_Expression,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Quantified_Expressions.Predicate);

   F105 : aliased constant Getter_Array :=
     (1 => (False, Parameter, F105_1'Access),
      2 => (False, Generalized_Iterator, F105_2'Access),
      3 => (False, Element_Iterator, F105_3'Access),
      4 => (False, Predicate, F105_4'Access));

   overriding procedure Quantified_Expression
    (Self    : in out Visitor;
     Element : not null Program.Elements.Quantified_Expressions
         .Quantified_Expression_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F105'Access;
   end Quantified_Expression;

   function F106_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Discriminant_Associations.Discriminant_Association,
      Vector        => Program.Elements.Identifiers.Identifier_Vector,
      Vector_Access => Program.Elements.Identifiers.Identifier_Vector_Access,
      Get_Vector    =>
        Program.Elements.Discriminant_Associations.Selector_Names);

   function F106_2 is new Generic_Child
     (Element      =>
        Program.Elements.Discriminant_Associations.Discriminant_Association,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Discriminant_Associations.Expression);

   F106 : aliased constant Getter_Array :=
     (1 => (True, Selector_Names, F106_1'Access),
      2 => (False, Expression, F106_2'Access));

   overriding procedure Discriminant_Association
    (Self    : in out Visitor;
     Element : not null Program.Elements.Discriminant_Associations
         .Discriminant_Association_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F106'Access;
   end Discriminant_Association;

   function F107_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Record_Component_Associations
          .Record_Component_Association,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Record_Component_Associations.Choices);

   function F107_2 is new Generic_Child
     (Element      =>
        Program.Elements.Record_Component_Associations
          .Record_Component_Association,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Record_Component_Associations.Expression);

   F107 : aliased constant Getter_Array :=
     (1 => (True, Choices, F107_1'Access),
      2 => (False, Expression, F107_2'Access));

   overriding procedure Record_Component_Association
    (Self    : in out Visitor;
     Element : not null Program.Elements.Record_Component_Associations
         .Record_Component_Association_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F107'Access;
   end Record_Component_Association;

   function F108_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Array_Component_Associations
          .Array_Component_Association,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Array_Component_Associations.Choices);

   function F108_2 is new Generic_Child
     (Element      =>
        Program.Elements.Array_Component_Associations
          .Array_Component_Association,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Array_Component_Associations.Expression);

   F108 : aliased constant Getter_Array :=
     (1 => (True, Choices, F108_1'Access),
      2 => (False, Expression, F108_2'Access));

   overriding procedure Array_Component_Association
    (Self    : in out Visitor;
     Element : not null Program.Elements.Array_Component_Associations
         .Array_Component_Association_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F108'Access;
   end Array_Component_Association;

   function F109_1 is new Generic_Child
     (Element      =>
        Program.Elements.Parameter_Associations.Parameter_Association,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Parameter_Associations.Formal_Parameter);

   function F109_2 is new Generic_Child
     (Element      =>
        Program.Elements.Parameter_Associations.Parameter_Association,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Parameter_Associations.Actual_Parameter);

   F109 : aliased constant Getter_Array :=
     (1 => (False, Formal_Parameter, F109_1'Access),
      2 => (False, Actual_Parameter, F109_2'Access));

   overriding procedure Parameter_Association
    (Self    : in out Visitor;
     Element : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F109'Access;
   end Parameter_Association;

   function F110_1 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Package_Associations
          .Formal_Package_Association,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Formal_Package_Associations.Formal_Parameter);

   function F110_2 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Package_Associations
          .Formal_Package_Association,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Formal_Package_Associations.Actual_Parameter);

   F110 : aliased constant Getter_Array :=
     (1 => (False, Formal_Parameter, F110_1'Access),
      2 => (False, Actual_Parameter, F110_2'Access));

   overriding procedure Formal_Package_Association
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Package_Associations
         .Formal_Package_Association_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F110'Access;
   end Formal_Package_Association;

   function F112_1 is new Generic_Child
     (Element      =>
        Program.Elements.Assignment_Statements.Assignment_Statement,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Assignment_Statements.Variable_Name);

   function F112_2 is new Generic_Child
     (Element      =>
        Program.Elements.Assignment_Statements.Assignment_Statement,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Assignment_Statements.Expression);

   F112 : aliased constant Getter_Array :=
     (1 => (False, Variable_Name, F112_1'Access),
      2 => (False, Expression, F112_2'Access));

   overriding procedure Assignment_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Assignment_Statements
         .Assignment_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F112'Access;
   end Assignment_Statement;

   function F113_1 is new Generic_Child
     (Element      => Program.Elements.If_Statements.If_Statement,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.If_Statements.Condition);

   function F113_2 is new Generic_Vector
     (Parent        => Program.Elements.If_Statements.If_Statement,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.If_Statements.Then_Statements);

   function F113_3 is new Generic_Vector
     (Parent        => Program.Elements.If_Statements.If_Statement,
      Vector        => Program.Elements.Elsif_Paths.Elsif_Path_Vector,
      Vector_Access => Program.Elements.Elsif_Paths.Elsif_Path_Vector_Access,
      Get_Vector    => Program.Elements.If_Statements.Elsif_Paths);

   function F113_4 is new Generic_Vector
     (Parent        => Program.Elements.If_Statements.If_Statement,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.If_Statements.Else_Statements);

   F113 : aliased constant Getter_Array :=
     (1 => (False, Condition, F113_1'Access),
      2 => (True, Then_Statements, F113_2'Access),
      3 => (True, Elsif_Paths, F113_3'Access),
      4 => (True, Else_Statements, F113_4'Access));

   overriding procedure If_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.If_Statements.If_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F113'Access;
   end If_Statement;

   function F114_1 is new Generic_Child
     (Element      => Program.Elements.Case_Statements.Case_Statement,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Case_Statements.Selecting_Expression);

   function F114_2 is new Generic_Vector
     (Parent        => Program.Elements.Case_Statements.Case_Statement,
      Vector        => Program.Elements.Case_Paths.Case_Path_Vector,
      Vector_Access => Program.Elements.Case_Paths.Case_Path_Vector_Access,
      Get_Vector    => Program.Elements.Case_Statements.Paths);

   F114 : aliased constant Getter_Array :=
     (1 => (False, Selecting_Expression, F114_1'Access),
      2 => (True, Paths, F114_2'Access));

   overriding procedure Case_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Case_Statements
         .Case_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F114'Access;
   end Case_Statement;

   function F115_1 is new Generic_Child
     (Element      => Program.Elements.Loop_Statements.Loop_Statement,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Loop_Statements.Statement_Identifier);

   function F115_2 is new Generic_Vector
     (Parent        => Program.Elements.Loop_Statements.Loop_Statement,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Loop_Statements.Statements);

   function F115_3 is new Generic_Child
     (Element      => Program.Elements.Loop_Statements.Loop_Statement,
      Child        => Program.Elements.Identifiers.Identifier,
      Child_Access => Program.Elements.Identifiers.Identifier_Access,
      Get_Child    =>
        Program.Elements.Loop_Statements.End_Statement_Identifier);

   F115 : aliased constant Getter_Array :=
     (1 => (False, Statement_Identifier, F115_1'Access),
      2 => (True, Statements, F115_2'Access),
      3 =>
        (False,
         End_Statement_Identifier,
         F115_3'Access));

   overriding procedure Loop_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Loop_Statements
         .Loop_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F115'Access;
   end Loop_Statement;

   function F116_1 is new Generic_Child
     (Element      =>
        Program.Elements.While_Loop_Statements.While_Loop_Statement,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    =>
        Program.Elements.While_Loop_Statements.Statement_Identifier);

   function F116_2 is new Generic_Child
     (Element      =>
        Program.Elements.While_Loop_Statements.While_Loop_Statement,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.While_Loop_Statements.Condition);

   function F116_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.While_Loop_Statements.While_Loop_Statement,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.While_Loop_Statements.Statements);

   function F116_4 is new Generic_Child
     (Element      =>
        Program.Elements.While_Loop_Statements.While_Loop_Statement,
      Child        => Program.Elements.Identifiers.Identifier,
      Child_Access => Program.Elements.Identifiers.Identifier_Access,
      Get_Child    =>
        Program.Elements.While_Loop_Statements.End_Statement_Identifier);

   F116 : aliased constant Getter_Array :=
     (1 => (False, Statement_Identifier, F116_1'Access),
      2 => (False, Condition, F116_2'Access),
      3 => (True, Statements, F116_3'Access),
      4 =>
        (False,
         End_Statement_Identifier,
         F116_4'Access));

   overriding procedure While_Loop_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.While_Loop_Statements
         .While_Loop_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F116'Access;
   end While_Loop_Statement;

   function F117_1 is new Generic_Child
     (Element      => Program.Elements.For_Loop_Statements.For_Loop_Statement,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    =>
        Program.Elements.For_Loop_Statements.Statement_Identifier);

   function F117_2 is new Generic_Child
     (Element      => Program.Elements.For_Loop_Statements.For_Loop_Statement,
      Child        =>
        Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification,
      Child_Access =>
        Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification_Access,
      Get_Child    => Program.Elements.For_Loop_Statements.Loop_Parameter);

   function F117_3 is new Generic_Child
     (Element      => Program.Elements.For_Loop_Statements.For_Loop_Statement,
      Child        =>
        Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification,
      Child_Access =>
        Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification_Access,
      Get_Child    =>
        Program.Elements.For_Loop_Statements.Generalized_Iterator);

   function F117_4 is new Generic_Child
     (Element      => Program.Elements.For_Loop_Statements.For_Loop_Statement,
      Child        =>
        Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification,
      Child_Access =>
        Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification_Access,
      Get_Child    => Program.Elements.For_Loop_Statements.Element_Iterator);

   function F117_5 is new Generic_Vector
     (Parent        => Program.Elements.For_Loop_Statements.For_Loop_Statement,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.For_Loop_Statements.Statements);

   function F117_6 is new Generic_Child
     (Element      => Program.Elements.For_Loop_Statements.For_Loop_Statement,
      Child        => Program.Elements.Identifiers.Identifier,
      Child_Access => Program.Elements.Identifiers.Identifier_Access,
      Get_Child    =>
        Program.Elements.For_Loop_Statements.End_Statement_Identifier);

   F117 : aliased constant Getter_Array :=
     (1 => (False, Statement_Identifier, F117_1'Access),
      2 => (False, Loop_Parameter, F117_2'Access),
      3 => (False, Generalized_Iterator, F117_3'Access),
      4 => (False, Element_Iterator, F117_4'Access),
      5 => (True, Statements, F117_5'Access),
      6 =>
        (False,
         End_Statement_Identifier,
         F117_6'Access));

   overriding procedure For_Loop_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.For_Loop_Statements
         .For_Loop_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F117'Access;
   end For_Loop_Statement;

   function F118_1 is new Generic_Child
     (Element      => Program.Elements.Block_Statements.Block_Statement,
      Child        =>
        Program.Elements.Defining_Identifiers.Defining_Identifier,
      Child_Access =>
        Program.Elements.Defining_Identifiers.Defining_Identifier_Access,
      Get_Child    => Program.Elements.Block_Statements.Statement_Identifier);

   function F118_2 is new Generic_Vector
     (Parent        => Program.Elements.Block_Statements.Block_Statement,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Block_Statements.Declarations);

   function F118_3 is new Generic_Vector
     (Parent        => Program.Elements.Block_Statements.Block_Statement,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Block_Statements.Statements);

   function F118_4 is new Generic_Vector
     (Parent        => Program.Elements.Block_Statements.Block_Statement,
      Vector        =>
        Program.Elements.Exception_Handlers.Exception_Handler_Vector,
      Vector_Access =>
        Program.Elements.Exception_Handlers.Exception_Handler_Vector_Access,
      Get_Vector    => Program.Elements.Block_Statements.Exception_Handlers);

   function F118_5 is new Generic_Child
     (Element      => Program.Elements.Block_Statements.Block_Statement,
      Child        => Program.Elements.Identifiers.Identifier,
      Child_Access => Program.Elements.Identifiers.Identifier_Access,
      Get_Child    =>
        Program.Elements.Block_Statements.End_Statement_Identifier);

   F118 : aliased constant Getter_Array :=
     (1 => (False, Statement_Identifier, F118_1'Access),
      2 => (True, Declarations, F118_2'Access),
      3 => (True, Statements, F118_3'Access),
      4 => (True, Exception_Handlers, F118_4'Access),
      5 =>
        (False,
         End_Statement_Identifier,
         F118_5'Access));

   overriding procedure Block_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Block_Statements
         .Block_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F118'Access;
   end Block_Statement;

   function F119_1 is new Generic_Child
     (Element      => Program.Elements.Exit_Statements.Exit_Statement,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Exit_Statements.Exit_Loop_Name);

   function F119_2 is new Generic_Child
     (Element      => Program.Elements.Exit_Statements.Exit_Statement,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Exit_Statements.Condition);

   F119 : aliased constant Getter_Array :=
     (1 => (False, Exit_Loop_Name, F119_1'Access),
      2 => (False, Condition, F119_2'Access));

   overriding procedure Exit_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Exit_Statements
         .Exit_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F119'Access;
   end Exit_Statement;

   function F120_1 is new Generic_Child
     (Element      => Program.Elements.Goto_Statements.Goto_Statement,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Goto_Statements.Goto_Label);

   F120 : aliased constant Getter_Array :=
     (1 => (False, Goto_Label, F120_1'Access));

   overriding procedure Goto_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Goto_Statements
         .Goto_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F120'Access;
   end Goto_Statement;

   function F121_1 is new Generic_Child
     (Element      => Program.Elements.Call_Statements.Call_Statement,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Call_Statements.Called_Name);

   function F121_2 is new Generic_Vector
     (Parent        => Program.Elements.Call_Statements.Call_Statement,
      Vector        =>
        Program.Elements.Parameter_Associations.Parameter_Association_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access,
      Get_Vector    => Program.Elements.Call_Statements.Parameters);

   F121 : aliased constant Getter_Array :=
     (1 => (False, Called_Name, F121_1'Access),
      2 => (True, Parameters, F121_2'Access));

   overriding procedure Call_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Call_Statements
         .Call_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F121'Access;
   end Call_Statement;

   function F122_1 is new Generic_Child
     (Element      =>
        Program.Elements.Simple_Return_Statements.Simple_Return_Statement,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Simple_Return_Statements.Expression);

   F122 : aliased constant Getter_Array :=
     (1 => (False, Expression, F122_1'Access));

   overriding procedure Simple_Return_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Simple_Return_Statements
         .Simple_Return_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F122'Access;
   end Simple_Return_Statement;

   function F123_1 is new Generic_Child
     (Element      =>
        Program.Elements.Extended_Return_Statements.Extended_Return_Statement,
      Child        =>
        Program.Elements.Return_Object_Specifications
          .Return_Object_Specification,
      Child_Access =>
        Program.Elements.Return_Object_Specifications
          .Return_Object_Specification_Access,
      Get_Child    =>
        Program.Elements.Extended_Return_Statements.Return_Object);

   function F123_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Extended_Return_Statements.Extended_Return_Statement,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Extended_Return_Statements.Statements);

   function F123_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Extended_Return_Statements.Extended_Return_Statement,
      Vector        =>
        Program.Elements.Exception_Handlers.Exception_Handler_Vector,
      Vector_Access =>
        Program.Elements.Exception_Handlers.Exception_Handler_Vector_Access,
      Get_Vector    =>
        Program.Elements.Extended_Return_Statements.Exception_Handlers);

   F123 : aliased constant Getter_Array :=
     (1 => (False, Return_Object, F123_1'Access),
      2 => (True, Statements, F123_2'Access),
      3 => (True, Exception_Handlers, F123_3'Access));

   overriding procedure Extended_Return_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Extended_Return_Statements
         .Extended_Return_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F123'Access;
   end Extended_Return_Statement;

   function F124_1 is new Generic_Child
     (Element      => Program.Elements.Accept_Statements.Accept_Statement,
      Child        => Program.Elements.Identifiers.Identifier,
      Child_Access => Program.Elements.Identifiers.Identifier_Access,
      Get_Child    => Program.Elements.Accept_Statements.Entry_Name);

   function F124_2 is new Generic_Child
     (Element      => Program.Elements.Accept_Statements.Accept_Statement,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Accept_Statements.Entry_Index);

   function F124_3 is new Generic_Vector
     (Parent        => Program.Elements.Accept_Statements.Accept_Statement,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Accept_Statements.Parameters);

   function F124_4 is new Generic_Vector
     (Parent        => Program.Elements.Accept_Statements.Accept_Statement,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Accept_Statements.Statements);

   function F124_5 is new Generic_Vector
     (Parent        => Program.Elements.Accept_Statements.Accept_Statement,
      Vector        =>
        Program.Elements.Exception_Handlers.Exception_Handler_Vector,
      Vector_Access =>
        Program.Elements.Exception_Handlers.Exception_Handler_Vector_Access,
      Get_Vector    => Program.Elements.Accept_Statements.Exception_Handlers);

   function F124_6 is new Generic_Child
     (Element      => Program.Elements.Accept_Statements.Accept_Statement,
      Child        => Program.Elements.Identifiers.Identifier,
      Child_Access => Program.Elements.Identifiers.Identifier_Access,
      Get_Child    =>
        Program.Elements.Accept_Statements.End_Statement_Identifier);

   F124 : aliased constant Getter_Array :=
     (1 => (False, Entry_Name, F124_1'Access),
      2 => (False, Entry_Index, F124_2'Access),
      3 => (True, Parameters, F124_3'Access),
      4 => (True, Statements, F124_4'Access),
      5 => (True, Exception_Handlers, F124_5'Access),
      6 =>
        (False,
         End_Statement_Identifier,
         F124_6'Access));

   overriding procedure Accept_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Accept_Statements
         .Accept_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F124'Access;
   end Accept_Statement;

   function F125_1 is new Generic_Child
     (Element      => Program.Elements.Requeue_Statements.Requeue_Statement,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Requeue_Statements.Entry_Name);

   F125 : aliased constant Getter_Array :=
     (1 => (False, Entry_Name, F125_1'Access));

   overriding procedure Requeue_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Requeue_Statements
         .Requeue_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F125'Access;
   end Requeue_Statement;

   function F126_1 is new Generic_Child
     (Element      => Program.Elements.Delay_Statements.Delay_Statement,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Delay_Statements.Expression);

   F126 : aliased constant Getter_Array :=
     (1 => (False, Expression, F126_1'Access));

   overriding procedure Delay_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Delay_Statements
         .Delay_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F126'Access;
   end Delay_Statement;

   function F128_1 is new Generic_Vector
     (Parent        => Program.Elements.Select_Statements.Select_Statement,
      Vector        => Program.Elements.Select_Paths.Select_Path_Vector,
      Vector_Access => Program.Elements.Select_Paths.Select_Path_Vector_Access,
      Get_Vector    => Program.Elements.Select_Statements.Paths);

   function F128_2 is new Generic_Vector
     (Parent        => Program.Elements.Select_Statements.Select_Statement,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    =>
        Program.Elements.Select_Statements.Then_Abort_Statements);

   function F128_3 is new Generic_Vector
     (Parent        => Program.Elements.Select_Statements.Select_Statement,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Select_Statements.Else_Statements);

   F128 : aliased constant Getter_Array :=
     (1 => (True, Paths, F128_1'Access),
      2 =>
        (True, Then_Abort_Statements, F128_2'Access),
      3 => (True, Else_Statements, F128_3'Access));

   overriding procedure Select_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Select_Statements
         .Select_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F128'Access;
   end Select_Statement;

   function F129_1 is new Generic_Vector
     (Parent        => Program.Elements.Abort_Statements.Abort_Statement,
      Vector        => Program.Elements.Expressions.Expression_Vector,
      Vector_Access => Program.Elements.Expressions.Expression_Vector_Access,
      Get_Vector    => Program.Elements.Abort_Statements.Aborted_Tasks);

   F129 : aliased constant Getter_Array :=
     (1 => (True, Aborted_Tasks, F129_1'Access));

   overriding procedure Abort_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Abort_Statements
         .Abort_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F129'Access;
   end Abort_Statement;

   function F130_1 is new Generic_Child
     (Element      => Program.Elements.Raise_Statements.Raise_Statement,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Raise_Statements.Raised_Exception);

   function F130_2 is new Generic_Child
     (Element      => Program.Elements.Raise_Statements.Raise_Statement,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Raise_Statements.Associated_Message);

   F130 : aliased constant Getter_Array :=
     (1 => (False, Raised_Exception, F130_1'Access),
      2 => (False, Associated_Message, F130_2'Access));

   overriding procedure Raise_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Raise_Statements
         .Raise_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F130'Access;
   end Raise_Statement;

   function F131_1 is new Generic_Child
     (Element      => Program.Elements.Code_Statements.Code_Statement,
      Child        =>
        Program.Elements.Qualified_Expressions.Qualified_Expression,
      Child_Access =>
        Program.Elements.Qualified_Expressions.Qualified_Expression_Access,
      Get_Child    => Program.Elements.Code_Statements.Expression);

   F131 : aliased constant Getter_Array :=
     (1 => (False, Expression, F131_1'Access));

   overriding procedure Code_Statement
    (Self    : in out Visitor;
     Element : not null Program.Elements.Code_Statements
         .Code_Statement_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F131'Access;
   end Code_Statement;

   function F132_1 is new Generic_Child
     (Element      => Program.Elements.Elsif_Paths.Elsif_Path,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Elsif_Paths.Condition);

   function F132_2 is new Generic_Vector
     (Parent        => Program.Elements.Elsif_Paths.Elsif_Path,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Elsif_Paths.Statements);

   F132 : aliased constant Getter_Array :=
     (1 => (False, Condition, F132_1'Access),
      2 => (True, Statements, F132_2'Access));

   overriding procedure Elsif_Path
    (Self    : in out Visitor;
     Element : not null Program.Elements.Elsif_Paths.Elsif_Path_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F132'Access;
   end Elsif_Path;

   function F133_1 is new Generic_Vector
     (Parent        => Program.Elements.Case_Paths.Case_Path,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Case_Paths.Choices);

   function F133_2 is new Generic_Vector
     (Parent        => Program.Elements.Case_Paths.Case_Path,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Case_Paths.Statements);

   F133 : aliased constant Getter_Array :=
     (1 => (True, Choices, F133_1'Access),
      2 => (True, Statements, F133_2'Access));

   overriding procedure Case_Path
    (Self    : in out Visitor;
     Element : not null Program.Elements.Case_Paths.Case_Path_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F133'Access;
   end Case_Path;

   function F134_1 is new Generic_Child
     (Element      => Program.Elements.Select_Paths.Select_Path,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Select_Paths.Guard);

   function F134_2 is new Generic_Vector
     (Parent        => Program.Elements.Select_Paths.Select_Path,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Select_Paths.Statements);

   F134 : aliased constant Getter_Array :=
     (1 => (False, Guard, F134_1'Access),
      2 => (True, Statements, F134_2'Access));

   overriding procedure Select_Path
    (Self    : in out Visitor;
     Element : not null Program.Elements.Select_Paths.Select_Path_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F134'Access;
   end Select_Path;

   function F135_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Case_Expression_Paths.Case_Expression_Path,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Case_Expression_Paths.Choices);

   function F135_2 is new Generic_Child
     (Element      =>
        Program.Elements.Case_Expression_Paths.Case_Expression_Path,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Case_Expression_Paths.Expression);

   F135 : aliased constant Getter_Array :=
     (1 => (True, Choices, F135_1'Access),
      2 => (False, Expression, F135_2'Access));

   overriding procedure Case_Expression_Path
    (Self    : in out Visitor;
     Element : not null Program.Elements.Case_Expression_Paths
         .Case_Expression_Path_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F135'Access;
   end Case_Expression_Path;

   function F136_1 is new Generic_Child
     (Element      =>
        Program.Elements.Elsif_Expression_Paths.Elsif_Expression_Path,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Elsif_Expression_Paths.Condition);

   function F136_2 is new Generic_Child
     (Element      =>
        Program.Elements.Elsif_Expression_Paths.Elsif_Expression_Path,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Elsif_Expression_Paths.Expression);

   F136 : aliased constant Getter_Array :=
     (1 => (False, Condition, F136_1'Access),
      2 => (False, Expression, F136_2'Access));

   overriding procedure Elsif_Expression_Path
    (Self    : in out Visitor;
     Element : not null Program.Elements.Elsif_Expression_Paths
         .Elsif_Expression_Path_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F136'Access;
   end Elsif_Expression_Path;

   function F137_1 is new Generic_Vector
     (Parent        => Program.Elements.Use_Clauses.Use_Clause,
      Vector        => Program.Elements.Expressions.Expression_Vector,
      Vector_Access => Program.Elements.Expressions.Expression_Vector_Access,
      Get_Vector    => Program.Elements.Use_Clauses.Clause_Names);

   F137 : aliased constant Getter_Array :=
     (1 => (True, Clause_Names, F137_1'Access));

   overriding procedure Use_Clause
    (Self    : in out Visitor;
     Element : not null Program.Elements.Use_Clauses.Use_Clause_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F137'Access;
   end Use_Clause;

   function F138_1 is new Generic_Vector
     (Parent        => Program.Elements.With_Clauses.With_Clause,
      Vector        => Program.Elements.Expressions.Expression_Vector,
      Vector_Access => Program.Elements.Expressions.Expression_Vector_Access,
      Get_Vector    => Program.Elements.With_Clauses.Clause_Names);

   F138 : aliased constant Getter_Array :=
     (1 => (True, Clause_Names, F138_1'Access));

   overriding procedure With_Clause
    (Self    : in out Visitor;
     Element : not null Program.Elements.With_Clauses.With_Clause_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F138'Access;
   end With_Clause;

   function F139_1 is new Generic_Child
     (Element      => Program.Elements.Component_Clauses.Component_Clause,
      Child        => Program.Elements.Identifiers.Identifier,
      Child_Access => Program.Elements.Identifiers.Identifier_Access,
      Get_Child    => Program.Elements.Component_Clauses.Clause_Name);

   function F139_2 is new Generic_Child
     (Element      => Program.Elements.Component_Clauses.Component_Clause,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Component_Clauses.Position);

   function F139_3 is new Generic_Child
     (Element      => Program.Elements.Component_Clauses.Component_Clause,
      Child        =>
        Program.Elements.Simple_Expression_Ranges.Simple_Expression_Range,
      Child_Access =>
        Program.Elements.Simple_Expression_Ranges
          .Simple_Expression_Range_Access,
      Get_Child    => Program.Elements.Component_Clauses.Clause_Range);

   F139 : aliased constant Getter_Array :=
     (1 => (False, Clause_Name, F139_1'Access),
      2 => (False, Position, F139_2'Access),
      3 => (False, Clause_Range, F139_3'Access));

   overriding procedure Component_Clause
    (Self    : in out Visitor;
     Element : not null Program.Elements.Component_Clauses
         .Component_Clause_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F139'Access;
   end Component_Clause;

   function F140_1 is new Generic_Child
     (Element      => Program.Elements.Derived_Types.Derived_Type,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Derived_Types.Parent);

   F140 : aliased constant Getter_Array :=
     (1 => (False, Parent, F140_1'Access));

   overriding procedure Derived_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Derived_Types.Derived_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F140'Access;
   end Derived_Type;

   function F141_1 is new Generic_Child
     (Element      =>
        Program.Elements.Derived_Record_Extensions.Derived_Record_Extension,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Derived_Record_Extensions.Parent);

   function F141_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Derived_Record_Extensions.Derived_Record_Extension,
      Vector        => Program.Elements.Expressions.Expression_Vector,
      Vector_Access => Program.Elements.Expressions.Expression_Vector_Access,
      Get_Vector    => Program.Elements.Derived_Record_Extensions.Progenitors);

   function F141_3 is new Generic_Child
     (Element      =>
        Program.Elements.Derived_Record_Extensions.Derived_Record_Extension,
      Child        => Program.Elements.Definitions.Definition,
      Child_Access => Program.Elements.Definitions.Definition_Access,
      Get_Child    =>
        Program.Elements.Derived_Record_Extensions.Record_Definition);

   F141 : aliased constant Getter_Array :=
     (1 => (False, Parent, F141_1'Access),
      2 => (True, Progenitors, F141_2'Access),
      3 => (False, Record_Definition, F141_3'Access));

   overriding procedure Derived_Record_Extension
    (Self    : in out Visitor;
     Element : not null Program.Elements.Derived_Record_Extensions
         .Derived_Record_Extension_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F141'Access;
   end Derived_Record_Extension;

   function F142_1 is new Generic_Vector
     (Parent        => Program.Elements.Enumeration_Types.Enumeration_Type,
      Vector        =>
        Program.Elements.Enumeration_Literal_Specifications
          .Enumeration_Literal_Specification_Vector,
      Vector_Access =>
        Program.Elements.Enumeration_Literal_Specifications
          .Enumeration_Literal_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Enumeration_Types.Literals);

   F142 : aliased constant Getter_Array :=
     (1 => (True, Literals, F142_1'Access));

   overriding procedure Enumeration_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Enumeration_Types
         .Enumeration_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F142'Access;
   end Enumeration_Type;

   function F143_1 is new Generic_Child
     (Element      =>
        Program.Elements.Signed_Integer_Types.Signed_Integer_Type,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Signed_Integer_Types.Lower_Bound);

   function F143_2 is new Generic_Child
     (Element      =>
        Program.Elements.Signed_Integer_Types.Signed_Integer_Type,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Signed_Integer_Types.Upper_Bound);

   F143 : aliased constant Getter_Array :=
     (1 => (False, Lower_Bound, F143_1'Access),
      2 => (False, Upper_Bound, F143_2'Access));

   overriding procedure Signed_Integer_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Signed_Integer_Types
         .Signed_Integer_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F143'Access;
   end Signed_Integer_Type;

   function F144_1 is new Generic_Child
     (Element      => Program.Elements.Modular_Types.Modular_Type,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Modular_Types.Modulus);

   F144 : aliased constant Getter_Array :=
     (1 => (False, Modulus, F144_1'Access));

   overriding procedure Modular_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Modular_Types.Modular_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F144'Access;
   end Modular_Type;

   function F146_1 is new Generic_Child
     (Element      =>
        Program.Elements.Floating_Point_Types.Floating_Point_Type,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Floating_Point_Types.Digits_Expression);

   function F146_2 is new Generic_Child
     (Element      =>
        Program.Elements.Floating_Point_Types.Floating_Point_Type,
      Child        =>
        Program.Elements.Real_Range_Specifications.Real_Range_Specification,
      Child_Access =>
        Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Access,
      Get_Child    => Program.Elements.Floating_Point_Types.Real_Range);

   F146 : aliased constant Getter_Array :=
     (1 => (False, Digits_Expression, F146_1'Access),
      2 => (False, Real_Range, F146_2'Access));

   overriding procedure Floating_Point_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Floating_Point_Types
         .Floating_Point_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F146'Access;
   end Floating_Point_Type;

   function F147_1 is new Generic_Child
     (Element      =>
        Program.Elements.Ordinary_Fixed_Point_Types.Ordinary_Fixed_Point_Type,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Ordinary_Fixed_Point_Types.Delta_Expression);

   function F147_2 is new Generic_Child
     (Element      =>
        Program.Elements.Ordinary_Fixed_Point_Types.Ordinary_Fixed_Point_Type,
      Child        =>
        Program.Elements.Real_Range_Specifications.Real_Range_Specification,
      Child_Access =>
        Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Access,
      Get_Child    => Program.Elements.Ordinary_Fixed_Point_Types.Real_Range);

   F147 : aliased constant Getter_Array :=
     (1 => (False, Delta_Expression, F147_1'Access),
      2 => (False, Real_Range, F147_2'Access));

   overriding procedure Ordinary_Fixed_Point_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Ordinary_Fixed_Point_Types
         .Ordinary_Fixed_Point_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F147'Access;
   end Ordinary_Fixed_Point_Type;

   function F148_1 is new Generic_Child
     (Element      =>
        Program.Elements.Decimal_Fixed_Point_Types.Decimal_Fixed_Point_Type,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Decimal_Fixed_Point_Types.Delta_Expression);

   function F148_2 is new Generic_Child
     (Element      =>
        Program.Elements.Decimal_Fixed_Point_Types.Decimal_Fixed_Point_Type,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Decimal_Fixed_Point_Types.Digits_Expression);

   function F148_3 is new Generic_Child
     (Element      =>
        Program.Elements.Decimal_Fixed_Point_Types.Decimal_Fixed_Point_Type,
      Child        =>
        Program.Elements.Real_Range_Specifications.Real_Range_Specification,
      Child_Access =>
        Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Access,
      Get_Child    => Program.Elements.Decimal_Fixed_Point_Types.Real_Range);

   F148 : aliased constant Getter_Array :=
     (1 => (False, Delta_Expression, F148_1'Access),
      2 => (False, Digits_Expression, F148_2'Access),
      3 => (False, Real_Range, F148_3'Access));

   overriding procedure Decimal_Fixed_Point_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Decimal_Fixed_Point_Types
         .Decimal_Fixed_Point_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F148'Access;
   end Decimal_Fixed_Point_Type;

   function F149_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Unconstrained_Array_Types.Unconstrained_Array_Type,
      Vector        => Program.Elements.Expressions.Expression_Vector,
      Vector_Access => Program.Elements.Expressions.Expression_Vector_Access,
      Get_Vector    =>
        Program.Elements.Unconstrained_Array_Types.Index_Subtypes);

   function F149_2 is new Generic_Child
     (Element      =>
        Program.Elements.Unconstrained_Array_Types.Unconstrained_Array_Type,
      Child        =>
        Program.Elements.Component_Definitions.Component_Definition,
      Child_Access =>
        Program.Elements.Component_Definitions.Component_Definition_Access,
      Get_Child    =>
        Program.Elements.Unconstrained_Array_Types.Component_Definition);

   F149 : aliased constant Getter_Array :=
     (1 => (True, Index_Subtypes, F149_1'Access),
      2 => (False, Component_Definition, F149_2'Access));

   overriding procedure Unconstrained_Array_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Unconstrained_Array_Types
         .Unconstrained_Array_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F149'Access;
   end Unconstrained_Array_Type;

   function F150_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Constrained_Array_Types.Constrained_Array_Type,
      Vector        => Program.Elements.Discrete_Ranges.Discrete_Range_Vector,
      Vector_Access =>
        Program.Elements.Discrete_Ranges.Discrete_Range_Vector_Access,
      Get_Vector    =>
        Program.Elements.Constrained_Array_Types.Index_Subtypes);

   function F150_2 is new Generic_Child
     (Element      =>
        Program.Elements.Constrained_Array_Types.Constrained_Array_Type,
      Child        =>
        Program.Elements.Component_Definitions.Component_Definition,
      Child_Access =>
        Program.Elements.Component_Definitions.Component_Definition_Access,
      Get_Child    =>
        Program.Elements.Constrained_Array_Types.Component_Definition);

   F150 : aliased constant Getter_Array :=
     (1 => (True, Index_Subtypes, F150_1'Access),
      2 => (False, Component_Definition, F150_2'Access));

   overriding procedure Constrained_Array_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Constrained_Array_Types
         .Constrained_Array_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F150'Access;
   end Constrained_Array_Type;

   function F151_1 is new Generic_Child
     (Element      => Program.Elements.Record_Types.Record_Type,
      Child        => Program.Elements.Definitions.Definition,
      Child_Access => Program.Elements.Definitions.Definition_Access,
      Get_Child    => Program.Elements.Record_Types.Record_Definition);

   F151 : aliased constant Getter_Array :=
     (1 => (False, Record_Definition, F151_1'Access));

   overriding procedure Record_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Record_Types.Record_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F151'Access;
   end Record_Type;

   function F152_1 is new Generic_Vector
     (Parent        => Program.Elements.Interface_Types.Interface_Type,
      Vector        => Program.Elements.Expressions.Expression_Vector,
      Vector_Access => Program.Elements.Expressions.Expression_Vector_Access,
      Get_Vector    => Program.Elements.Interface_Types.Progenitors);

   F152 : aliased constant Getter_Array :=
     (1 => (True, Progenitors, F152_1'Access));

   overriding procedure Interface_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Interface_Types
         .Interface_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F152'Access;
   end Interface_Type;

   function F153_1 is new Generic_Child
     (Element      => Program.Elements.Object_Access_Types.Object_Access_Type,
      Child        => Program.Elements.Subtype_Indications.Subtype_Indication,
      Child_Access =>
        Program.Elements.Subtype_Indications.Subtype_Indication_Access,
      Get_Child    => Program.Elements.Object_Access_Types.Subtype_Indication);

   F153 : aliased constant Getter_Array :=
     (1 => (False, Subtype_Indication, F153_1'Access));

   overriding procedure Object_Access_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Object_Access_Types
         .Object_Access_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F153'Access;
   end Object_Access_Type;

   function F154_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Procedure_Access_Types.Procedure_Access_Type,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Procedure_Access_Types.Parameters);

   F154 : aliased constant Getter_Array :=
     (1 => (True, Parameters, F154_1'Access));

   overriding procedure Procedure_Access_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Procedure_Access_Types
         .Procedure_Access_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F154'Access;
   end Procedure_Access_Type;

   function F155_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Function_Access_Types.Function_Access_Type,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    => Program.Elements.Function_Access_Types.Parameters);

   function F155_2 is new Generic_Child
     (Element      =>
        Program.Elements.Function_Access_Types.Function_Access_Type,
      Child        => Program.Elements.Element,
      Child_Access => Program.Elements.Element_Access,
      Get_Child    => Program.Elements.Function_Access_Types.Result_Subtype);

   F155 : aliased constant Getter_Array :=
     (1 => (True, Parameters, F155_1'Access),
      2 => (False, Result_Subtype, F155_2'Access));

   overriding procedure Function_Access_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Function_Access_Types
         .Function_Access_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F155'Access;
   end Function_Access_Type;

   function F157_1 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Derived_Type_Definitions
          .Formal_Derived_Type_Definition,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Formal_Derived_Type_Definitions.Subtype_Mark);

   function F157_2 is new Generic_Vector
     (Parent        =>
        Program.Elements.Formal_Derived_Type_Definitions
          .Formal_Derived_Type_Definition,
      Vector        => Program.Elements.Expressions.Expression_Vector,
      Vector_Access => Program.Elements.Expressions.Expression_Vector_Access,
      Get_Vector    =>
        Program.Elements.Formal_Derived_Type_Definitions.Progenitors);

   F157 : aliased constant Getter_Array :=
     (1 => (False, Subtype_Mark, F157_1'Access),
      2 => (True, Progenitors, F157_2'Access));

   overriding procedure Formal_Derived_Type_Definition
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Derived_Type_Definitions
         .Formal_Derived_Type_Definition_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F157'Access;
   end Formal_Derived_Type_Definition;

   function F164_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Formal_Unconstrained_Array_Types
          .Formal_Unconstrained_Array_Type,
      Vector        => Program.Elements.Expressions.Expression_Vector,
      Vector_Access => Program.Elements.Expressions.Expression_Vector_Access,
      Get_Vector    =>
        Program.Elements.Formal_Unconstrained_Array_Types.Index_Subtypes);

   function F164_2 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Unconstrained_Array_Types
          .Formal_Unconstrained_Array_Type,
      Child        =>
        Program.Elements.Component_Definitions.Component_Definition,
      Child_Access =>
        Program.Elements.Component_Definitions.Component_Definition_Access,
      Get_Child    =>
        Program.Elements.Formal_Unconstrained_Array_Types
          .Component_Definition);

   F164 : aliased constant Getter_Array :=
     (1 => (True, Index_Subtypes, F164_1'Access),
      2 => (False, Component_Definition, F164_2'Access));

   overriding procedure Formal_Unconstrained_Array_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Unconstrained_Array_Types
         .Formal_Unconstrained_Array_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F164'Access;
   end Formal_Unconstrained_Array_Type;

   function F165_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Formal_Constrained_Array_Types
          .Formal_Constrained_Array_Type,
      Vector        => Program.Elements.Discrete_Ranges.Discrete_Range_Vector,
      Vector_Access =>
        Program.Elements.Discrete_Ranges.Discrete_Range_Vector_Access,
      Get_Vector    =>
        Program.Elements.Formal_Constrained_Array_Types.Index_Subtypes);

   function F165_2 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Constrained_Array_Types
          .Formal_Constrained_Array_Type,
      Child        =>
        Program.Elements.Component_Definitions.Component_Definition,
      Child_Access =>
        Program.Elements.Component_Definitions.Component_Definition_Access,
      Get_Child    =>
        Program.Elements.Formal_Constrained_Array_Types.Component_Definition);

   F165 : aliased constant Getter_Array :=
     (1 => (True, Index_Subtypes, F165_1'Access),
      2 => (False, Component_Definition, F165_2'Access));

   overriding procedure Formal_Constrained_Array_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Constrained_Array_Types
         .Formal_Constrained_Array_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F165'Access;
   end Formal_Constrained_Array_Type;

   function F166_1 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Object_Access_Types.Formal_Object_Access_Type,
      Child        => Program.Elements.Subtype_Indications.Subtype_Indication,
      Child_Access =>
        Program.Elements.Subtype_Indications.Subtype_Indication_Access,
      Get_Child    =>
        Program.Elements.Formal_Object_Access_Types.Subtype_Indication);

   F166 : aliased constant Getter_Array :=
     (1 => (False, Subtype_Indication, F166_1'Access));

   overriding procedure Formal_Object_Access_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Object_Access_Types
         .Formal_Object_Access_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F166'Access;
   end Formal_Object_Access_Type;

   function F167_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Formal_Procedure_Access_Types
          .Formal_Procedure_Access_Type,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Formal_Procedure_Access_Types.Parameters);

   F167 : aliased constant Getter_Array :=
     (1 => (True, Parameters, F167_1'Access));

   overriding procedure Formal_Procedure_Access_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Procedure_Access_Types
         .Formal_Procedure_Access_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F167'Access;
   end Formal_Procedure_Access_Type;

   function F168_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Formal_Function_Access_Types
          .Formal_Function_Access_Type,
      Vector        =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector,
      Vector_Access =>
        Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access,
      Get_Vector    =>
        Program.Elements.Formal_Function_Access_Types.Parameters);

   function F168_2 is new Generic_Child
     (Element      =>
        Program.Elements.Formal_Function_Access_Types
          .Formal_Function_Access_Type,
      Child        => Program.Elements.Element,
      Child_Access => Program.Elements.Element_Access,
      Get_Child    =>
        Program.Elements.Formal_Function_Access_Types.Result_Subtype);

   F168 : aliased constant Getter_Array :=
     (1 => (True, Parameters, F168_1'Access),
      2 => (False, Result_Subtype, F168_2'Access));

   overriding procedure Formal_Function_Access_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Function_Access_Types
         .Formal_Function_Access_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F168'Access;
   end Formal_Function_Access_Type;

   function F169_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Formal_Interface_Types.Formal_Interface_Type,
      Vector        => Program.Elements.Expressions.Expression_Vector,
      Vector_Access => Program.Elements.Expressions.Expression_Vector_Access,
      Get_Vector    => Program.Elements.Formal_Interface_Types.Progenitors);

   F169 : aliased constant Getter_Array :=
     (1 => (True, Progenitors, F169_1'Access));

   overriding procedure Formal_Interface_Type
    (Self    : in out Visitor;
     Element : not null Program.Elements.Formal_Interface_Types
         .Formal_Interface_Type_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F169'Access;
   end Formal_Interface_Type;

   function F170_1 is new Generic_Child
     (Element      =>
        Program.Elements.Range_Attribute_References.Range_Attribute_Reference,
      Child        =>
        Program.Elements.Attribute_References.Attribute_Reference,
      Child_Access =>
        Program.Elements.Attribute_References.Attribute_Reference_Access,
      Get_Child    =>
        Program.Elements.Range_Attribute_References.Range_Attribute);

   F170 : aliased constant Getter_Array :=
     (1 => (False, Range_Attribute, F170_1'Access));

   overriding procedure Range_Attribute_Reference
    (Self    : in out Visitor;
     Element : not null Program.Elements.Range_Attribute_References
         .Range_Attribute_Reference_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F170'Access;
   end Range_Attribute_Reference;

   function F171_1 is new Generic_Child
     (Element      =>
        Program.Elements.Simple_Expression_Ranges.Simple_Expression_Range,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Simple_Expression_Ranges.Lower_Bound);

   function F171_2 is new Generic_Child
     (Element      =>
        Program.Elements.Simple_Expression_Ranges.Simple_Expression_Range,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Simple_Expression_Ranges.Upper_Bound);

   F171 : aliased constant Getter_Array :=
     (1 => (False, Lower_Bound, F171_1'Access),
      2 => (False, Upper_Bound, F171_2'Access));

   overriding procedure Simple_Expression_Range
    (Self    : in out Visitor;
     Element : not null Program.Elements.Simple_Expression_Ranges
         .Simple_Expression_Range_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F171'Access;
   end Simple_Expression_Range;

   function F172_1 is new Generic_Child
     (Element      => Program.Elements.Digits_Constraints.Digits_Constraint,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Digits_Constraints.Digits_Expression);

   function F172_2 is new Generic_Child
     (Element      => Program.Elements.Digits_Constraints.Digits_Constraint,
      Child        => Program.Elements.Constraints.Constraint,
      Child_Access => Program.Elements.Constraints.Constraint_Access,
      Get_Child    =>
        Program.Elements.Digits_Constraints.Real_Range_Constraint);

   F172 : aliased constant Getter_Array :=
     (1 => (False, Digits_Expression, F172_1'Access),
      2 =>
        (False, Real_Range_Constraint, F172_2'Access));

   overriding procedure Digits_Constraint
    (Self    : in out Visitor;
     Element : not null Program.Elements.Digits_Constraints
         .Digits_Constraint_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F172'Access;
   end Digits_Constraint;

   function F173_1 is new Generic_Child
     (Element      => Program.Elements.Delta_Constraints.Delta_Constraint,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Delta_Constraints.Delta_Expression);

   function F173_2 is new Generic_Child
     (Element      => Program.Elements.Delta_Constraints.Delta_Constraint,
      Child        => Program.Elements.Constraints.Constraint,
      Child_Access => Program.Elements.Constraints.Constraint_Access,
      Get_Child    =>
        Program.Elements.Delta_Constraints.Real_Range_Constraint);

   F173 : aliased constant Getter_Array :=
     (1 => (False, Delta_Expression, F173_1'Access),
      2 =>
        (False, Real_Range_Constraint, F173_2'Access));

   overriding procedure Delta_Constraint
    (Self    : in out Visitor;
     Element : not null Program.Elements.Delta_Constraints
         .Delta_Constraint_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F173'Access;
   end Delta_Constraint;

   function F174_1 is new Generic_Vector
     (Parent        => Program.Elements.Index_Constraints.Index_Constraint,
      Vector        => Program.Elements.Discrete_Ranges.Discrete_Range_Vector,
      Vector_Access =>
        Program.Elements.Discrete_Ranges.Discrete_Range_Vector_Access,
      Get_Vector    => Program.Elements.Index_Constraints.Ranges);

   F174 : aliased constant Getter_Array :=
     (1 => (True, Ranges, F174_1'Access));

   overriding procedure Index_Constraint
    (Self    : in out Visitor;
     Element : not null Program.Elements.Index_Constraints
         .Index_Constraint_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F174'Access;
   end Index_Constraint;

   function F175_1 is new Generic_Vector
     (Parent        =>
        Program.Elements.Discriminant_Constraints.Discriminant_Constraint,
      Vector        =>
        Program.Elements.Discriminant_Associations
          .Discriminant_Association_Vector,
      Vector_Access =>
        Program.Elements.Discriminant_Associations
          .Discriminant_Association_Vector_Access,
      Get_Vector    =>
        Program.Elements.Discriminant_Constraints.Discriminants);

   F175 : aliased constant Getter_Array :=
     (1 => (True, Discriminants, F175_1'Access));

   overriding procedure Discriminant_Constraint
    (Self    : in out Visitor;
     Element : not null Program.Elements.Discriminant_Constraints
         .Discriminant_Constraint_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F175'Access;
   end Discriminant_Constraint;

   function F176_1 is new Generic_Child
     (Element      =>
        Program.Elements.Attribute_Definition_Clauses
          .Attribute_Definition_Clause,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Attribute_Definition_Clauses.Name);

   function F176_2 is new Generic_Child
     (Element      =>
        Program.Elements.Attribute_Definition_Clauses
          .Attribute_Definition_Clause,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Attribute_Definition_Clauses.Expression);

   F176 : aliased constant Getter_Array :=
     (1 => (False, Name, F176_1'Access),
      2 => (False, Expression, F176_2'Access));

   overriding procedure Attribute_Definition_Clause
    (Self    : in out Visitor;
     Element : not null Program.Elements.Attribute_Definition_Clauses
         .Attribute_Definition_Clause_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F176'Access;
   end Attribute_Definition_Clause;

   function F177_1 is new Generic_Child
     (Element      =>
        Program.Elements.Enumeration_Representation_Clauses
          .Enumeration_Representation_Clause,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Enumeration_Representation_Clauses.Name);

   function F177_2 is new Generic_Child
     (Element      =>
        Program.Elements.Enumeration_Representation_Clauses
          .Enumeration_Representation_Clause,
      Child        => Program.Elements.Array_Aggregates.Array_Aggregate,
      Child_Access => Program.Elements.Array_Aggregates.Array_Aggregate_Access,
      Get_Child    =>
        Program.Elements.Enumeration_Representation_Clauses.Expression);

   F177 : aliased constant Getter_Array :=
     (1 => (False, Name, F177_1'Access),
      2 => (False, Expression, F177_2'Access));

   overriding procedure Enumeration_Representation_Clause
    (Self    : in out Visitor;
     Element : not null Program.Elements.Enumeration_Representation_Clauses
         .Enumeration_Representation_Clause_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F177'Access;
   end Enumeration_Representation_Clause;

   function F178_1 is new Generic_Child
     (Element      =>
        Program.Elements.Record_Representation_Clauses
          .Record_Representation_Clause,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.Record_Representation_Clauses.Name);

   function F178_2 is new Generic_Child
     (Element      =>
        Program.Elements.Record_Representation_Clauses
          .Record_Representation_Clause,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    =>
        Program.Elements.Record_Representation_Clauses.Mod_Clause_Expression);

   function F178_3 is new Generic_Vector
     (Parent        =>
        Program.Elements.Record_Representation_Clauses
          .Record_Representation_Clause,
      Vector        =>
        Program.Elements.Component_Clauses.Component_Clause_Vector,
      Vector_Access =>
        Program.Elements.Component_Clauses.Component_Clause_Vector_Access,
      Get_Vector    =>
        Program.Elements.Record_Representation_Clauses.Component_Clauses);

   F178 : aliased constant Getter_Array :=
     (1 => (False, Name, F178_1'Access),
      2 =>
        (False, Mod_Clause_Expression, F178_2'Access),
      3 => (True, Component_Clauses, F178_3'Access));

   overriding procedure Record_Representation_Clause
    (Self    : in out Visitor;
     Element : not null Program.Elements.Record_Representation_Clauses
         .Record_Representation_Clause_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F178'Access;
   end Record_Representation_Clause;

   function F179_1 is new Generic_Child
     (Element      => Program.Elements.At_Clauses.At_Clause,
      Child        => Program.Elements.Identifiers.Identifier,
      Child_Access => Program.Elements.Identifiers.Identifier_Access,
      Get_Child    => Program.Elements.At_Clauses.Name);

   function F179_2 is new Generic_Child
     (Element      => Program.Elements.At_Clauses.At_Clause,
      Child        => Program.Elements.Expressions.Expression,
      Child_Access => Program.Elements.Expressions.Expression_Access,
      Get_Child    => Program.Elements.At_Clauses.Expression);

   F179 : aliased constant Getter_Array :=
     (1 => (False, Name, F179_1'Access),
      2 => (False, Expression, F179_2'Access));

   overriding procedure At_Clause
    (Self    : in out Visitor;
     Element : not null Program.Elements.At_Clauses.At_Clause_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F179'Access;
   end At_Clause;

   function F180_1 is new Generic_Child
     (Element      => Program.Elements.Exception_Handlers.Exception_Handler,
      Child        =>
        Program.Elements.Choice_Parameter_Specifications
          .Choice_Parameter_Specification,
      Child_Access =>
        Program.Elements.Choice_Parameter_Specifications
          .Choice_Parameter_Specification_Access,
      Get_Child    => Program.Elements.Exception_Handlers.Choice_Parameter);

   function F180_2 is new Generic_Vector
     (Parent        => Program.Elements.Exception_Handlers.Exception_Handler,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Exception_Handlers.Choices);

   function F180_3 is new Generic_Vector
     (Parent        => Program.Elements.Exception_Handlers.Exception_Handler,
      Vector        => Program.Element_Vectors.Element_Vector,
      Vector_Access => Program.Element_Vectors.Element_Vector_Access,
      Get_Vector    => Program.Elements.Exception_Handlers.Statements);

   F180 : aliased constant Getter_Array :=
     (1 => (False, Choice_Parameter, F180_1'Access),
      2 => (True, Choices, F180_2'Access),
      3 => (True, Statements, F180_3'Access));

   overriding procedure Exception_Handler
    (Self    : in out Visitor;
     Element : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Access) is
      pragma Unreferenced (Element);
   begin
      Self.Result := F180'Access;
   end Exception_Handler;

   function Get
    (Parent : Program.Elements.Element_Access)
      return access constant Getter_Array is
      V : Visitor;
   begin
      Parent.Visit (V);
      return V.Result;
   end Get;

end Internal;
