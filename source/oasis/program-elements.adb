--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Pragmas;
with Program.Elements.Defining_Names;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Defining_Character_Literals;
with Program.Elements.Defining_Operator_Symbols;
with Program.Elements.Defining_Expanded_Names;
with Program.Elements.Declarations;
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
with Program.Elements.Type_Definitions;
with Program.Elements.Subtype_Indications;
with Program.Elements.Constraints;
with Program.Elements.Component_Definitions;
with Program.Elements.Discrete_Subtype_Definitions;
with Program.Elements.Discrete_Ranges;
with Program.Elements.Unknown_Discriminant_Parts;
with Program.Elements.Known_Discriminant_Parts;
with Program.Elements.Record_Definitions;
with Program.Elements.Null_Components;
with Program.Elements.Variant_Parts;
with Program.Elements.Variants;
with Program.Elements.Others_Choices;
with Program.Elements.Anonymous_Access_Definitions;
with Program.Elements.Private_Type_Definitions;
with Program.Elements.Private_Extension_Definitions;
with Program.Elements.Incomplete_Type_Definitions;
with Program.Elements.Task_Definitions;
with Program.Elements.Protected_Definitions;
with Program.Elements.Formal_Type_Definitions;
with Program.Elements.Aspect_Specifications;
with Program.Elements.Real_Range_Specifications;
with Program.Elements.Expressions;
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
with Program.Elements.Statements;
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
with Program.Elements.Representation_Clauses;
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
with Program.Elements.Formal_Access_Types;
with Program.Elements.Access_Types;
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

package body Program.Elements is

   function Each_Enclosing_Element
    (Self : not null access Element'Class)
      return Program.Element_Iterators.Enclosing_Element_Iterator
     renames Program.Element_Iterators.To_Enclosing_Element_Iterator;

   function Each_Child
    (Self : not null access Element'Class)
      return Program.Element_Iterators.Child_Iterator
     renames Program.Element_Iterators.To_Child_Iterator;

   function To_Pragma
    (Self : access Element'Class)
      return Program.Elements.Pragmas.Pragma_Access is
   begin
      return Program.Elements.Pragmas.Pragma_Access (Self);
   end To_Pragma;

   function To_Defining_Name
    (Self : access Element'Class)
      return Program.Elements.Defining_Names.Defining_Name_Access is
   begin
      return Program.Elements.Defining_Names.Defining_Name_Access (Self);
   end To_Defining_Name;

   function To_Defining_Identifier
    (Self : access Element'Class)
      return Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
        (Self);
   end To_Defining_Identifier;

   function To_Defining_Character_Literal
    (Self : access Element'Class)
      return Program.Elements.Defining_Character_Literals
          .Defining_Character_Literal_Access is
   begin
      return Program.Elements.Defining_Character_Literals
        .Defining_Character_Literal_Access
        (Self);
   end To_Defining_Character_Literal;

   function To_Defining_Operator_Symbol
    (Self : access Element'Class)
      return Program.Elements.Defining_Operator_Symbols
          .Defining_Operator_Symbol_Access is
   begin
      return Program.Elements.Defining_Operator_Symbols
        .Defining_Operator_Symbol_Access
        (Self);
   end To_Defining_Operator_Symbol;

   function To_Defining_Expanded_Name
    (Self : access Element'Class)
      return Program.Elements.Defining_Expanded_Names
          .Defining_Expanded_Name_Access is
   begin
      return Program.Elements.Defining_Expanded_Names
        .Defining_Expanded_Name_Access
        (Self);
   end To_Defining_Expanded_Name;

   function To_Declaration
    (Self : access Element'Class)
      return Program.Elements.Declarations.Declaration_Access is
   begin
      return Program.Elements.Declarations.Declaration_Access (Self);
   end To_Declaration;

   function To_Type_Declaration
    (Self : access Element'Class)
      return Program.Elements.Type_Declarations.Type_Declaration_Access is
   begin
      return Program.Elements.Type_Declarations.Type_Declaration_Access (Self);
   end To_Type_Declaration;

   function To_Task_Type_Declaration
    (Self : access Element'Class)
      return Program.Elements.Task_Type_Declarations
          .Task_Type_Declaration_Access is
   begin
      return Program.Elements.Task_Type_Declarations
        .Task_Type_Declaration_Access
        (Self);
   end To_Task_Type_Declaration;

   function To_Protected_Type_Declaration
    (Self : access Element'Class)
      return Program.Elements.Protected_Type_Declarations
          .Protected_Type_Declaration_Access is
   begin
      return Program.Elements.Protected_Type_Declarations
        .Protected_Type_Declaration_Access
        (Self);
   end To_Protected_Type_Declaration;

   function To_Subtype_Declaration
    (Self : access Element'Class)
      return Program.Elements.Subtype_Declarations
          .Subtype_Declaration_Access is
   begin
      return Program.Elements.Subtype_Declarations.Subtype_Declaration_Access
        (Self);
   end To_Subtype_Declaration;

   function To_Object_Declaration
    (Self : access Element'Class)
      return Program.Elements.Object_Declarations.Object_Declaration_Access is
   begin
      return Program.Elements.Object_Declarations.Object_Declaration_Access
        (Self);
   end To_Object_Declaration;

   function To_Single_Task_Declaration
    (Self : access Element'Class)
      return Program.Elements.Single_Task_Declarations
          .Single_Task_Declaration_Access is
   begin
      return Program.Elements.Single_Task_Declarations
        .Single_Task_Declaration_Access
        (Self);
   end To_Single_Task_Declaration;

   function To_Single_Protected_Declaration
    (Self : access Element'Class)
      return Program.Elements.Single_Protected_Declarations
          .Single_Protected_Declaration_Access is
   begin
      return Program.Elements.Single_Protected_Declarations
        .Single_Protected_Declaration_Access
        (Self);
   end To_Single_Protected_Declaration;

   function To_Number_Declaration
    (Self : access Element'Class)
      return Program.Elements.Number_Declarations.Number_Declaration_Access is
   begin
      return Program.Elements.Number_Declarations.Number_Declaration_Access
        (Self);
   end To_Number_Declaration;

   function To_Enumeration_Literal_Specification
    (Self : access Element'Class)
      return Program.Elements.Enumeration_Literal_Specifications
          .Enumeration_Literal_Specification_Access is
   begin
      return Program.Elements.Enumeration_Literal_Specifications
        .Enumeration_Literal_Specification_Access
        (Self);
   end To_Enumeration_Literal_Specification;

   function To_Discriminant_Specification
    (Self : access Element'Class)
      return Program.Elements.Discriminant_Specifications
          .Discriminant_Specification_Access is
   begin
      return Program.Elements.Discriminant_Specifications
        .Discriminant_Specification_Access
        (Self);
   end To_Discriminant_Specification;

   function To_Component_Declaration
    (Self : access Element'Class)
      return Program.Elements.Component_Declarations
          .Component_Declaration_Access is
   begin
      return Program.Elements.Component_Declarations
        .Component_Declaration_Access
        (Self);
   end To_Component_Declaration;

   function To_Loop_Parameter_Specification
    (Self : access Element'Class)
      return Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification_Access is
   begin
      return Program.Elements.Loop_Parameter_Specifications
        .Loop_Parameter_Specification_Access
        (Self);
   end To_Loop_Parameter_Specification;

   function To_Generalized_Iterator_Specification
    (Self : access Element'Class)
      return Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification_Access is
   begin
      return Program.Elements.Generalized_Iterator_Specifications
        .Generalized_Iterator_Specification_Access
        (Self);
   end To_Generalized_Iterator_Specification;

   function To_Element_Iterator_Specification
    (Self : access Element'Class)
      return Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification_Access is
   begin
      return Program.Elements.Element_Iterator_Specifications
        .Element_Iterator_Specification_Access
        (Self);
   end To_Element_Iterator_Specification;

   function To_Procedure_Declaration
    (Self : access Element'Class)
      return Program.Elements.Procedure_Declarations
          .Procedure_Declaration_Access is
   begin
      return Program.Elements.Procedure_Declarations
        .Procedure_Declaration_Access
        (Self);
   end To_Procedure_Declaration;

   function To_Function_Declaration
    (Self : access Element'Class)
      return Program.Elements.Function_Declarations
          .Function_Declaration_Access is
   begin
      return Program.Elements.Function_Declarations.Function_Declaration_Access
        (Self);
   end To_Function_Declaration;

   function To_Parameter_Specification
    (Self : access Element'Class)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Access is
   begin
      return Program.Elements.Parameter_Specifications
        .Parameter_Specification_Access
        (Self);
   end To_Parameter_Specification;

   function To_Procedure_Body_Declaration
    (Self : access Element'Class)
      return Program.Elements.Procedure_Body_Declarations
          .Procedure_Body_Declaration_Access is
   begin
      return Program.Elements.Procedure_Body_Declarations
        .Procedure_Body_Declaration_Access
        (Self);
   end To_Procedure_Body_Declaration;

   function To_Function_Body_Declaration
    (Self : access Element'Class)
      return Program.Elements.Function_Body_Declarations
          .Function_Body_Declaration_Access is
   begin
      return Program.Elements.Function_Body_Declarations
        .Function_Body_Declaration_Access
        (Self);
   end To_Function_Body_Declaration;

   function To_Return_Object_Specification
    (Self : access Element'Class)
      return Program.Elements.Return_Object_Specifications
          .Return_Object_Specification_Access is
   begin
      return Program.Elements.Return_Object_Specifications
        .Return_Object_Specification_Access
        (Self);
   end To_Return_Object_Specification;

   function To_Package_Declaration
    (Self : access Element'Class)
      return Program.Elements.Package_Declarations
          .Package_Declaration_Access is
   begin
      return Program.Elements.Package_Declarations.Package_Declaration_Access
        (Self);
   end To_Package_Declaration;

   function To_Package_Body_Declaration
    (Self : access Element'Class)
      return Program.Elements.Package_Body_Declarations
          .Package_Body_Declaration_Access is
   begin
      return Program.Elements.Package_Body_Declarations
        .Package_Body_Declaration_Access
        (Self);
   end To_Package_Body_Declaration;

   function To_Object_Renaming_Declaration
    (Self : access Element'Class)
      return Program.Elements.Object_Renaming_Declarations
          .Object_Renaming_Declaration_Access is
   begin
      return Program.Elements.Object_Renaming_Declarations
        .Object_Renaming_Declaration_Access
        (Self);
   end To_Object_Renaming_Declaration;

   function To_Exception_Renaming_Declaration
    (Self : access Element'Class)
      return Program.Elements.Exception_Renaming_Declarations
          .Exception_Renaming_Declaration_Access is
   begin
      return Program.Elements.Exception_Renaming_Declarations
        .Exception_Renaming_Declaration_Access
        (Self);
   end To_Exception_Renaming_Declaration;

   function To_Procedure_Renaming_Declaration
    (Self : access Element'Class)
      return Program.Elements.Procedure_Renaming_Declarations
          .Procedure_Renaming_Declaration_Access is
   begin
      return Program.Elements.Procedure_Renaming_Declarations
        .Procedure_Renaming_Declaration_Access
        (Self);
   end To_Procedure_Renaming_Declaration;

   function To_Function_Renaming_Declaration
    (Self : access Element'Class)
      return Program.Elements.Function_Renaming_Declarations
          .Function_Renaming_Declaration_Access is
   begin
      return Program.Elements.Function_Renaming_Declarations
        .Function_Renaming_Declaration_Access
        (Self);
   end To_Function_Renaming_Declaration;

   function To_Package_Renaming_Declaration
    (Self : access Element'Class)
      return Program.Elements.Package_Renaming_Declarations
          .Package_Renaming_Declaration_Access is
   begin
      return Program.Elements.Package_Renaming_Declarations
        .Package_Renaming_Declaration_Access
        (Self);
   end To_Package_Renaming_Declaration;

   function To_Generic_Package_Renaming_Declaration
    (Self : access Element'Class)
      return Program.Elements.Generic_Package_Renaming_Declarations
          .Generic_Package_Renaming_Declaration_Access is
   begin
      return Program.Elements.Generic_Package_Renaming_Declarations
        .Generic_Package_Renaming_Declaration_Access
        (Self);
   end To_Generic_Package_Renaming_Declaration;

   function To_Generic_Procedure_Renaming_Declaration
    (Self : access Element'Class)
      return Program.Elements.Generic_Procedure_Renaming_Declarations
          .Generic_Procedure_Renaming_Declaration_Access is
   begin
      return Program.Elements.Generic_Procedure_Renaming_Declarations
        .Generic_Procedure_Renaming_Declaration_Access
        (Self);
   end To_Generic_Procedure_Renaming_Declaration;

   function To_Generic_Function_Renaming_Declaration
    (Self : access Element'Class)
      return Program.Elements.Generic_Function_Renaming_Declarations
          .Generic_Function_Renaming_Declaration_Access is
   begin
      return Program.Elements.Generic_Function_Renaming_Declarations
        .Generic_Function_Renaming_Declaration_Access
        (Self);
   end To_Generic_Function_Renaming_Declaration;

   function To_Task_Body_Declaration
    (Self : access Element'Class)
      return Program.Elements.Task_Body_Declarations
          .Task_Body_Declaration_Access is
   begin
      return Program.Elements.Task_Body_Declarations
        .Task_Body_Declaration_Access
        (Self);
   end To_Task_Body_Declaration;

   function To_Protected_Body_Declaration
    (Self : access Element'Class)
      return Program.Elements.Protected_Body_Declarations
          .Protected_Body_Declaration_Access is
   begin
      return Program.Elements.Protected_Body_Declarations
        .Protected_Body_Declaration_Access
        (Self);
   end To_Protected_Body_Declaration;

   function To_Entry_Declaration
    (Self : access Element'Class)
      return Program.Elements.Entry_Declarations.Entry_Declaration_Access is
   begin
      return Program.Elements.Entry_Declarations.Entry_Declaration_Access
        (Self);
   end To_Entry_Declaration;

   function To_Entry_Body_Declaration
    (Self : access Element'Class)
      return Program.Elements.Entry_Body_Declarations
          .Entry_Body_Declaration_Access is
   begin
      return Program.Elements.Entry_Body_Declarations
        .Entry_Body_Declaration_Access
        (Self);
   end To_Entry_Body_Declaration;

   function To_Entry_Index_Specification
    (Self : access Element'Class)
      return Program.Elements.Entry_Index_Specifications
          .Entry_Index_Specification_Access is
   begin
      return Program.Elements.Entry_Index_Specifications
        .Entry_Index_Specification_Access
        (Self);
   end To_Entry_Index_Specification;

   function To_Procedure_Body_Stub
    (Self : access Element'Class)
      return Program.Elements.Procedure_Body_Stubs
          .Procedure_Body_Stub_Access is
   begin
      return Program.Elements.Procedure_Body_Stubs.Procedure_Body_Stub_Access
        (Self);
   end To_Procedure_Body_Stub;

   function To_Function_Body_Stub
    (Self : access Element'Class)
      return Program.Elements.Function_Body_Stubs.Function_Body_Stub_Access is
   begin
      return Program.Elements.Function_Body_Stubs.Function_Body_Stub_Access
        (Self);
   end To_Function_Body_Stub;

   function To_Package_Body_Stub
    (Self : access Element'Class)
      return Program.Elements.Package_Body_Stubs.Package_Body_Stub_Access is
   begin
      return Program.Elements.Package_Body_Stubs.Package_Body_Stub_Access
        (Self);
   end To_Package_Body_Stub;

   function To_Task_Body_Stub
    (Self : access Element'Class)
      return Program.Elements.Task_Body_Stubs.Task_Body_Stub_Access is
   begin
      return Program.Elements.Task_Body_Stubs.Task_Body_Stub_Access (Self);
   end To_Task_Body_Stub;

   function To_Protected_Body_Stub
    (Self : access Element'Class)
      return Program.Elements.Protected_Body_Stubs
          .Protected_Body_Stub_Access is
   begin
      return Program.Elements.Protected_Body_Stubs.Protected_Body_Stub_Access
        (Self);
   end To_Protected_Body_Stub;

   function To_Exception_Declaration
    (Self : access Element'Class)
      return Program.Elements.Exception_Declarations
          .Exception_Declaration_Access is
   begin
      return Program.Elements.Exception_Declarations
        .Exception_Declaration_Access
        (Self);
   end To_Exception_Declaration;

   function To_Choice_Parameter_Specification
    (Self : access Element'Class)
      return Program.Elements.Choice_Parameter_Specifications
          .Choice_Parameter_Specification_Access is
   begin
      return Program.Elements.Choice_Parameter_Specifications
        .Choice_Parameter_Specification_Access
        (Self);
   end To_Choice_Parameter_Specification;

   function To_Generic_Package_Declaration
    (Self : access Element'Class)
      return Program.Elements.Generic_Package_Declarations
          .Generic_Package_Declaration_Access is
   begin
      return Program.Elements.Generic_Package_Declarations
        .Generic_Package_Declaration_Access
        (Self);
   end To_Generic_Package_Declaration;

   function To_Generic_Procedure_Declaration
    (Self : access Element'Class)
      return Program.Elements.Generic_Procedure_Declarations
          .Generic_Procedure_Declaration_Access is
   begin
      return Program.Elements.Generic_Procedure_Declarations
        .Generic_Procedure_Declaration_Access
        (Self);
   end To_Generic_Procedure_Declaration;

   function To_Generic_Function_Declaration
    (Self : access Element'Class)
      return Program.Elements.Generic_Function_Declarations
          .Generic_Function_Declaration_Access is
   begin
      return Program.Elements.Generic_Function_Declarations
        .Generic_Function_Declaration_Access
        (Self);
   end To_Generic_Function_Declaration;

   function To_Package_Instantiation
    (Self : access Element'Class)
      return Program.Elements.Package_Instantiations
          .Package_Instantiation_Access is
   begin
      return Program.Elements.Package_Instantiations
        .Package_Instantiation_Access
        (Self);
   end To_Package_Instantiation;

   function To_Procedure_Instantiation
    (Self : access Element'Class)
      return Program.Elements.Procedure_Instantiations
          .Procedure_Instantiation_Access is
   begin
      return Program.Elements.Procedure_Instantiations
        .Procedure_Instantiation_Access
        (Self);
   end To_Procedure_Instantiation;

   function To_Function_Instantiation
    (Self : access Element'Class)
      return Program.Elements.Function_Instantiations
          .Function_Instantiation_Access is
   begin
      return Program.Elements.Function_Instantiations
        .Function_Instantiation_Access
        (Self);
   end To_Function_Instantiation;

   function To_Formal_Object_Declaration
    (Self : access Element'Class)
      return Program.Elements.Formal_Object_Declarations
          .Formal_Object_Declaration_Access is
   begin
      return Program.Elements.Formal_Object_Declarations
        .Formal_Object_Declaration_Access
        (Self);
   end To_Formal_Object_Declaration;

   function To_Formal_Type_Declaration
    (Self : access Element'Class)
      return Program.Elements.Formal_Type_Declarations
          .Formal_Type_Declaration_Access is
   begin
      return Program.Elements.Formal_Type_Declarations
        .Formal_Type_Declaration_Access
        (Self);
   end To_Formal_Type_Declaration;

   function To_Formal_Procedure_Declaration
    (Self : access Element'Class)
      return Program.Elements.Formal_Procedure_Declarations
          .Formal_Procedure_Declaration_Access is
   begin
      return Program.Elements.Formal_Procedure_Declarations
        .Formal_Procedure_Declaration_Access
        (Self);
   end To_Formal_Procedure_Declaration;

   function To_Formal_Function_Declaration
    (Self : access Element'Class)
      return Program.Elements.Formal_Function_Declarations
          .Formal_Function_Declaration_Access is
   begin
      return Program.Elements.Formal_Function_Declarations
        .Formal_Function_Declaration_Access
        (Self);
   end To_Formal_Function_Declaration;

   function To_Formal_Package_Declaration
    (Self : access Element'Class)
      return Program.Elements.Formal_Package_Declarations
          .Formal_Package_Declaration_Access is
   begin
      return Program.Elements.Formal_Package_Declarations
        .Formal_Package_Declaration_Access
        (Self);
   end To_Formal_Package_Declaration;

   function To_Definition
    (Self : access Element'Class)
      return Program.Elements.Definitions.Definition_Access is
   begin
      return Program.Elements.Definitions.Definition_Access (Self);
   end To_Definition;

   function To_Type_Definition
    (Self : access Element'Class)
      return Program.Elements.Type_Definitions.Type_Definition_Access is
   begin
      return Program.Elements.Type_Definitions.Type_Definition_Access (Self);
   end To_Type_Definition;

   function To_Subtype_Indication
    (Self : access Element'Class)
      return Program.Elements.Subtype_Indications.Subtype_Indication_Access is
   begin
      return Program.Elements.Subtype_Indications.Subtype_Indication_Access
        (Self);
   end To_Subtype_Indication;

   function To_Constraint
    (Self : access Element'Class)
      return Program.Elements.Constraints.Constraint_Access is
   begin
      return Program.Elements.Constraints.Constraint_Access (Self);
   end To_Constraint;

   function To_Component_Definition
    (Self : access Element'Class)
      return Program.Elements.Component_Definitions
          .Component_Definition_Access is
   begin
      return Program.Elements.Component_Definitions.Component_Definition_Access
        (Self);
   end To_Component_Definition;

   function To_Discrete_Subtype_Definition
    (Self : access Element'Class)
      return Program.Elements.Discrete_Subtype_Definitions
          .Discrete_Subtype_Definition_Access is
   begin
      return Program.Elements.Discrete_Subtype_Definitions
        .Discrete_Subtype_Definition_Access
        (Self);
   end To_Discrete_Subtype_Definition;

   function To_Discrete_Range
    (Self : access Element'Class)
      return Program.Elements.Discrete_Ranges.Discrete_Range_Access is
   begin
      return Program.Elements.Discrete_Ranges.Discrete_Range_Access (Self);
   end To_Discrete_Range;

   function To_Unknown_Discriminant_Part
    (Self : access Element'Class)
      return Program.Elements.Unknown_Discriminant_Parts
          .Unknown_Discriminant_Part_Access is
   begin
      return Program.Elements.Unknown_Discriminant_Parts
        .Unknown_Discriminant_Part_Access
        (Self);
   end To_Unknown_Discriminant_Part;

   function To_Known_Discriminant_Part
    (Self : access Element'Class)
      return Program.Elements.Known_Discriminant_Parts
          .Known_Discriminant_Part_Access is
   begin
      return Program.Elements.Known_Discriminant_Parts
        .Known_Discriminant_Part_Access
        (Self);
   end To_Known_Discriminant_Part;

   function To_Record_Definition
    (Self : access Element'Class)
      return Program.Elements.Record_Definitions.Record_Definition_Access is
   begin
      return Program.Elements.Record_Definitions.Record_Definition_Access
        (Self);
   end To_Record_Definition;

   function To_Null_Component
    (Self : access Element'Class)
      return Program.Elements.Null_Components.Null_Component_Access is
   begin
      return Program.Elements.Null_Components.Null_Component_Access (Self);
   end To_Null_Component;

   function To_Variant_Part
    (Self : access Element'Class)
      return Program.Elements.Variant_Parts.Variant_Part_Access is
   begin
      return Program.Elements.Variant_Parts.Variant_Part_Access (Self);
   end To_Variant_Part;

   function To_Variant
    (Self : access Element'Class)
      return Program.Elements.Variants.Variant_Access is
   begin
      return Program.Elements.Variants.Variant_Access (Self);
   end To_Variant;

   function To_Others_Choice
    (Self : access Element'Class)
      return Program.Elements.Others_Choices.Others_Choice_Access is
   begin
      return Program.Elements.Others_Choices.Others_Choice_Access (Self);
   end To_Others_Choice;

   function To_Anonymous_Access_Definition
    (Self : access Element'Class)
      return Program.Elements.Anonymous_Access_Definitions
          .Anonymous_Access_Definition_Access is
   begin
      return Program.Elements.Anonymous_Access_Definitions
        .Anonymous_Access_Definition_Access
        (Self);
   end To_Anonymous_Access_Definition;

   function To_Private_Type_Definition
    (Self : access Element'Class)
      return Program.Elements.Private_Type_Definitions
          .Private_Type_Definition_Access is
   begin
      return Program.Elements.Private_Type_Definitions
        .Private_Type_Definition_Access
        (Self);
   end To_Private_Type_Definition;

   function To_Private_Extension_Definition
    (Self : access Element'Class)
      return Program.Elements.Private_Extension_Definitions
          .Private_Extension_Definition_Access is
   begin
      return Program.Elements.Private_Extension_Definitions
        .Private_Extension_Definition_Access
        (Self);
   end To_Private_Extension_Definition;

   function To_Incomplete_Type_Definition
    (Self : access Element'Class)
      return Program.Elements.Incomplete_Type_Definitions
          .Incomplete_Type_Definition_Access is
   begin
      return Program.Elements.Incomplete_Type_Definitions
        .Incomplete_Type_Definition_Access
        (Self);
   end To_Incomplete_Type_Definition;

   function To_Task_Definition
    (Self : access Element'Class)
      return Program.Elements.Task_Definitions.Task_Definition_Access is
   begin
      return Program.Elements.Task_Definitions.Task_Definition_Access (Self);
   end To_Task_Definition;

   function To_Protected_Definition
    (Self : access Element'Class)
      return Program.Elements.Protected_Definitions
          .Protected_Definition_Access is
   begin
      return Program.Elements.Protected_Definitions.Protected_Definition_Access
        (Self);
   end To_Protected_Definition;

   function To_Formal_Type_Definition
    (Self : access Element'Class)
      return Program.Elements.Formal_Type_Definitions
          .Formal_Type_Definition_Access is
   begin
      return Program.Elements.Formal_Type_Definitions
        .Formal_Type_Definition_Access
        (Self);
   end To_Formal_Type_Definition;

   function To_Aspect_Specification
    (Self : access Element'Class)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Access is
   begin
      return Program.Elements.Aspect_Specifications.Aspect_Specification_Access
        (Self);
   end To_Aspect_Specification;

   function To_Real_Range_Specification
    (Self : access Element'Class)
      return Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Access is
   begin
      return Program.Elements.Real_Range_Specifications
        .Real_Range_Specification_Access
        (Self);
   end To_Real_Range_Specification;

   function To_Expression
    (Self : access Element'Class)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Program.Elements.Expressions.Expression_Access (Self);
   end To_Expression;

   function To_Numeric_Literal
    (Self : access Element'Class)
      return Program.Elements.Numeric_Literals.Numeric_Literal_Access is
   begin
      return Program.Elements.Numeric_Literals.Numeric_Literal_Access (Self);
   end To_Numeric_Literal;

   function To_String_Literal
    (Self : access Element'Class)
      return Program.Elements.String_Literals.String_Literal_Access is
   begin
      return Program.Elements.String_Literals.String_Literal_Access (Self);
   end To_String_Literal;

   function To_Identifier
    (Self : access Element'Class)
      return Program.Elements.Identifiers.Identifier_Access is
   begin
      return Program.Elements.Identifiers.Identifier_Access (Self);
   end To_Identifier;

   function To_Operator_Symbol
    (Self : access Element'Class)
      return Program.Elements.Operator_Symbols.Operator_Symbol_Access is
   begin
      return Program.Elements.Operator_Symbols.Operator_Symbol_Access (Self);
   end To_Operator_Symbol;

   function To_Character_Literal
    (Self : access Element'Class)
      return Program.Elements.Character_Literals.Character_Literal_Access is
   begin
      return Program.Elements.Character_Literals.Character_Literal_Access
        (Self);
   end To_Character_Literal;

   function To_Explicit_Dereference
    (Self : access Element'Class)
      return Program.Elements.Explicit_Dereferences
          .Explicit_Dereference_Access is
   begin
      return Program.Elements.Explicit_Dereferences.Explicit_Dereference_Access
        (Self);
   end To_Explicit_Dereference;

   function To_Function_Call
    (Self : access Element'Class)
      return Program.Elements.Function_Calls.Function_Call_Access is
   begin
      return Program.Elements.Function_Calls.Function_Call_Access (Self);
   end To_Function_Call;

   function To_Indexed_Component
    (Self : access Element'Class)
      return Program.Elements.Indexed_Components.Indexed_Component_Access is
   begin
      return Program.Elements.Indexed_Components.Indexed_Component_Access
        (Self);
   end To_Indexed_Component;

   function To_Slice
    (Self : access Element'Class)
      return Program.Elements.Slices.Slice_Access is
   begin
      return Program.Elements.Slices.Slice_Access (Self);
   end To_Slice;

   function To_Selected_Component
    (Self : access Element'Class)
      return Program.Elements.Selected_Components.Selected_Component_Access is
   begin
      return Program.Elements.Selected_Components.Selected_Component_Access
        (Self);
   end To_Selected_Component;

   function To_Attribute_Reference
    (Self : access Element'Class)
      return Program.Elements.Attribute_References
          .Attribute_Reference_Access is
   begin
      return Program.Elements.Attribute_References.Attribute_Reference_Access
        (Self);
   end To_Attribute_Reference;

   function To_Record_Aggregate
    (Self : access Element'Class)
      return Program.Elements.Record_Aggregates.Record_Aggregate_Access is
   begin
      return Program.Elements.Record_Aggregates.Record_Aggregate_Access (Self);
   end To_Record_Aggregate;

   function To_Extension_Aggregate
    (Self : access Element'Class)
      return Program.Elements.Extension_Aggregates
          .Extension_Aggregate_Access is
   begin
      return Program.Elements.Extension_Aggregates.Extension_Aggregate_Access
        (Self);
   end To_Extension_Aggregate;

   function To_Array_Aggregate
    (Self : access Element'Class)
      return Program.Elements.Array_Aggregates.Array_Aggregate_Access is
   begin
      return Program.Elements.Array_Aggregates.Array_Aggregate_Access (Self);
   end To_Array_Aggregate;

   function To_Short_Circuit_Operation
    (Self : access Element'Class)
      return Program.Elements.Short_Circuit_Operations
          .Short_Circuit_Operation_Access is
   begin
      return Program.Elements.Short_Circuit_Operations
        .Short_Circuit_Operation_Access
        (Self);
   end To_Short_Circuit_Operation;

   function To_Membership_Test
    (Self : access Element'Class)
      return Program.Elements.Membership_Tests.Membership_Test_Access is
   begin
      return Program.Elements.Membership_Tests.Membership_Test_Access (Self);
   end To_Membership_Test;

   function To_Null_Literal
    (Self : access Element'Class)
      return Program.Elements.Null_Literals.Null_Literal_Access is
   begin
      return Program.Elements.Null_Literals.Null_Literal_Access (Self);
   end To_Null_Literal;

   function To_Parenthesized_Expression
    (Self : access Element'Class)
      return Program.Elements.Parenthesized_Expressions
          .Parenthesized_Expression_Access is
   begin
      return Program.Elements.Parenthesized_Expressions
        .Parenthesized_Expression_Access
        (Self);
   end To_Parenthesized_Expression;

   function To_Raise_Expression
    (Self : access Element'Class)
      return Program.Elements.Raise_Expressions.Raise_Expression_Access is
   begin
      return Program.Elements.Raise_Expressions.Raise_Expression_Access (Self);
   end To_Raise_Expression;

   function To_Type_Conversion
    (Self : access Element'Class)
      return Program.Elements.Type_Conversions.Type_Conversion_Access is
   begin
      return Program.Elements.Type_Conversions.Type_Conversion_Access (Self);
   end To_Type_Conversion;

   function To_Qualified_Expression
    (Self : access Element'Class)
      return Program.Elements.Qualified_Expressions
          .Qualified_Expression_Access is
   begin
      return Program.Elements.Qualified_Expressions.Qualified_Expression_Access
        (Self);
   end To_Qualified_Expression;

   function To_Allocator
    (Self : access Element'Class)
      return Program.Elements.Allocators.Allocator_Access is
   begin
      return Program.Elements.Allocators.Allocator_Access (Self);
   end To_Allocator;

   function To_Case_Expression
    (Self : access Element'Class)
      return Program.Elements.Case_Expressions.Case_Expression_Access is
   begin
      return Program.Elements.Case_Expressions.Case_Expression_Access (Self);
   end To_Case_Expression;

   function To_If_Expression
    (Self : access Element'Class)
      return Program.Elements.If_Expressions.If_Expression_Access is
   begin
      return Program.Elements.If_Expressions.If_Expression_Access (Self);
   end To_If_Expression;

   function To_Quantified_Expression
    (Self : access Element'Class)
      return Program.Elements.Quantified_Expressions
          .Quantified_Expression_Access is
   begin
      return Program.Elements.Quantified_Expressions
        .Quantified_Expression_Access
        (Self);
   end To_Quantified_Expression;

   function To_Association
    (Self : access Element'Class)
      return Program.Elements.Associations.Association_Access is
   begin
      return Program.Elements.Associations.Association_Access (Self);
   end To_Association;

   function To_Discriminant_Association
    (Self : access Element'Class)
      return Program.Elements.Discriminant_Associations
          .Discriminant_Association_Access is
   begin
      return Program.Elements.Discriminant_Associations
        .Discriminant_Association_Access
        (Self);
   end To_Discriminant_Association;

   function To_Record_Component_Association
    (Self : access Element'Class)
      return Program.Elements.Record_Component_Associations
          .Record_Component_Association_Access is
   begin
      return Program.Elements.Record_Component_Associations
        .Record_Component_Association_Access
        (Self);
   end To_Record_Component_Association;

   function To_Array_Component_Association
    (Self : access Element'Class)
      return Program.Elements.Array_Component_Associations
          .Array_Component_Association_Access is
   begin
      return Program.Elements.Array_Component_Associations
        .Array_Component_Association_Access
        (Self);
   end To_Array_Component_Association;

   function To_Parameter_Association
    (Self : access Element'Class)
      return Program.Elements.Parameter_Associations
          .Parameter_Association_Access is
   begin
      return Program.Elements.Parameter_Associations
        .Parameter_Association_Access
        (Self);
   end To_Parameter_Association;

   function To_Formal_Package_Association
    (Self : access Element'Class)
      return Program.Elements.Formal_Package_Associations
          .Formal_Package_Association_Access is
   begin
      return Program.Elements.Formal_Package_Associations
        .Formal_Package_Association_Access
        (Self);
   end To_Formal_Package_Association;

   function To_Statement
    (Self : access Element'Class)
      return Program.Elements.Statements.Statement_Access is
   begin
      return Program.Elements.Statements.Statement_Access (Self);
   end To_Statement;

   function To_Null_Statement
    (Self : access Element'Class)
      return Program.Elements.Null_Statements.Null_Statement_Access is
   begin
      return Program.Elements.Null_Statements.Null_Statement_Access (Self);
   end To_Null_Statement;

   function To_Assignment_Statement
    (Self : access Element'Class)
      return Program.Elements.Assignment_Statements
          .Assignment_Statement_Access is
   begin
      return Program.Elements.Assignment_Statements.Assignment_Statement_Access
        (Self);
   end To_Assignment_Statement;

   function To_If_Statement
    (Self : access Element'Class)
      return Program.Elements.If_Statements.If_Statement_Access is
   begin
      return Program.Elements.If_Statements.If_Statement_Access (Self);
   end To_If_Statement;

   function To_Case_Statement
    (Self : access Element'Class)
      return Program.Elements.Case_Statements.Case_Statement_Access is
   begin
      return Program.Elements.Case_Statements.Case_Statement_Access (Self);
   end To_Case_Statement;

   function To_Loop_Statement
    (Self : access Element'Class)
      return Program.Elements.Loop_Statements.Loop_Statement_Access is
   begin
      return Program.Elements.Loop_Statements.Loop_Statement_Access (Self);
   end To_Loop_Statement;

   function To_While_Loop_Statement
    (Self : access Element'Class)
      return Program.Elements.While_Loop_Statements
          .While_Loop_Statement_Access is
   begin
      return Program.Elements.While_Loop_Statements.While_Loop_Statement_Access
        (Self);
   end To_While_Loop_Statement;

   function To_For_Loop_Statement
    (Self : access Element'Class)
      return Program.Elements.For_Loop_Statements.For_Loop_Statement_Access is
   begin
      return Program.Elements.For_Loop_Statements.For_Loop_Statement_Access
        (Self);
   end To_For_Loop_Statement;

   function To_Block_Statement
    (Self : access Element'Class)
      return Program.Elements.Block_Statements.Block_Statement_Access is
   begin
      return Program.Elements.Block_Statements.Block_Statement_Access (Self);
   end To_Block_Statement;

   function To_Exit_Statement
    (Self : access Element'Class)
      return Program.Elements.Exit_Statements.Exit_Statement_Access is
   begin
      return Program.Elements.Exit_Statements.Exit_Statement_Access (Self);
   end To_Exit_Statement;

   function To_Goto_Statement
    (Self : access Element'Class)
      return Program.Elements.Goto_Statements.Goto_Statement_Access is
   begin
      return Program.Elements.Goto_Statements.Goto_Statement_Access (Self);
   end To_Goto_Statement;

   function To_Call_Statement
    (Self : access Element'Class)
      return Program.Elements.Call_Statements.Call_Statement_Access is
   begin
      return Program.Elements.Call_Statements.Call_Statement_Access (Self);
   end To_Call_Statement;

   function To_Simple_Return_Statement
    (Self : access Element'Class)
      return Program.Elements.Simple_Return_Statements
          .Simple_Return_Statement_Access is
   begin
      return Program.Elements.Simple_Return_Statements
        .Simple_Return_Statement_Access
        (Self);
   end To_Simple_Return_Statement;

   function To_Extended_Return_Statement
    (Self : access Element'Class)
      return Program.Elements.Extended_Return_Statements
          .Extended_Return_Statement_Access is
   begin
      return Program.Elements.Extended_Return_Statements
        .Extended_Return_Statement_Access
        (Self);
   end To_Extended_Return_Statement;

   function To_Accept_Statement
    (Self : access Element'Class)
      return Program.Elements.Accept_Statements.Accept_Statement_Access is
   begin
      return Program.Elements.Accept_Statements.Accept_Statement_Access (Self);
   end To_Accept_Statement;

   function To_Requeue_Statement
    (Self : access Element'Class)
      return Program.Elements.Requeue_Statements.Requeue_Statement_Access is
   begin
      return Program.Elements.Requeue_Statements.Requeue_Statement_Access
        (Self);
   end To_Requeue_Statement;

   function To_Delay_Statement
    (Self : access Element'Class)
      return Program.Elements.Delay_Statements.Delay_Statement_Access is
   begin
      return Program.Elements.Delay_Statements.Delay_Statement_Access (Self);
   end To_Delay_Statement;

   function To_Terminate_Alternative_Statement
    (Self : access Element'Class)
      return Program.Elements.Terminate_Alternative_Statements
          .Terminate_Alternative_Statement_Access is
   begin
      return Program.Elements.Terminate_Alternative_Statements
        .Terminate_Alternative_Statement_Access
        (Self);
   end To_Terminate_Alternative_Statement;

   function To_Select_Statement
    (Self : access Element'Class)
      return Program.Elements.Select_Statements.Select_Statement_Access is
   begin
      return Program.Elements.Select_Statements.Select_Statement_Access (Self);
   end To_Select_Statement;

   function To_Abort_Statement
    (Self : access Element'Class)
      return Program.Elements.Abort_Statements.Abort_Statement_Access is
   begin
      return Program.Elements.Abort_Statements.Abort_Statement_Access (Self);
   end To_Abort_Statement;

   function To_Raise_Statement
    (Self : access Element'Class)
      return Program.Elements.Raise_Statements.Raise_Statement_Access is
   begin
      return Program.Elements.Raise_Statements.Raise_Statement_Access (Self);
   end To_Raise_Statement;

   function To_Code_Statement
    (Self : access Element'Class)
      return Program.Elements.Code_Statements.Code_Statement_Access is
   begin
      return Program.Elements.Code_Statements.Code_Statement_Access (Self);
   end To_Code_Statement;

   function To_Path
    (Self : access Element'Class)
      return Program.Elements.Paths.Path_Access is
   begin
      return Program.Elements.Paths.Path_Access (Self);
   end To_Path;

   function To_Elsif_Path
    (Self : access Element'Class)
      return Program.Elements.Elsif_Paths.Elsif_Path_Access is
   begin
      return Program.Elements.Elsif_Paths.Elsif_Path_Access (Self);
   end To_Elsif_Path;

   function To_Case_Path
    (Self : access Element'Class)
      return Program.Elements.Case_Paths.Case_Path_Access is
   begin
      return Program.Elements.Case_Paths.Case_Path_Access (Self);
   end To_Case_Path;

   function To_Select_Path
    (Self : access Element'Class)
      return Program.Elements.Select_Paths.Select_Path_Access is
   begin
      return Program.Elements.Select_Paths.Select_Path_Access (Self);
   end To_Select_Path;

   function To_Case_Expression_Path
    (Self : access Element'Class)
      return Program.Elements.Case_Expression_Paths
          .Case_Expression_Path_Access is
   begin
      return Program.Elements.Case_Expression_Paths.Case_Expression_Path_Access
        (Self);
   end To_Case_Expression_Path;

   function To_Elsif_Expression_Path
    (Self : access Element'Class)
      return Program.Elements.Elsif_Expression_Paths
          .Elsif_Expression_Path_Access is
   begin
      return Program.Elements.Elsif_Expression_Paths
        .Elsif_Expression_Path_Access
        (Self);
   end To_Elsif_Expression_Path;

   function To_Clause
    (Self : access Element'Class)
      return Program.Elements.Clauses.Clause_Access is
   begin
      return Program.Elements.Clauses.Clause_Access (Self);
   end To_Clause;

   function To_Use_Clause
    (Self : access Element'Class)
      return Program.Elements.Use_Clauses.Use_Clause_Access is
   begin
      return Program.Elements.Use_Clauses.Use_Clause_Access (Self);
   end To_Use_Clause;

   function To_With_Clause
    (Self : access Element'Class)
      return Program.Elements.With_Clauses.With_Clause_Access is
   begin
      return Program.Elements.With_Clauses.With_Clause_Access (Self);
   end To_With_Clause;

   function To_Representation_Clause
    (Self : access Element'Class)
      return Program.Elements.Representation_Clauses
          .Representation_Clause_Access is
   begin
      return Program.Elements.Representation_Clauses
        .Representation_Clause_Access
        (Self);
   end To_Representation_Clause;

   function To_Component_Clause
    (Self : access Element'Class)
      return Program.Elements.Component_Clauses.Component_Clause_Access is
   begin
      return Program.Elements.Component_Clauses.Component_Clause_Access (Self);
   end To_Component_Clause;

   function To_Derived_Type
    (Self : access Element'Class)
      return Program.Elements.Derived_Types.Derived_Type_Access is
   begin
      return Program.Elements.Derived_Types.Derived_Type_Access (Self);
   end To_Derived_Type;

   function To_Derived_Record_Extension
    (Self : access Element'Class)
      return Program.Elements.Derived_Record_Extensions
          .Derived_Record_Extension_Access is
   begin
      return Program.Elements.Derived_Record_Extensions
        .Derived_Record_Extension_Access
        (Self);
   end To_Derived_Record_Extension;

   function To_Enumeration_Type
    (Self : access Element'Class)
      return Program.Elements.Enumeration_Types.Enumeration_Type_Access is
   begin
      return Program.Elements.Enumeration_Types.Enumeration_Type_Access (Self);
   end To_Enumeration_Type;

   function To_Signed_Integer_Type
    (Self : access Element'Class)
      return Program.Elements.Signed_Integer_Types
          .Signed_Integer_Type_Access is
   begin
      return Program.Elements.Signed_Integer_Types.Signed_Integer_Type_Access
        (Self);
   end To_Signed_Integer_Type;

   function To_Modular_Type
    (Self : access Element'Class)
      return Program.Elements.Modular_Types.Modular_Type_Access is
   begin
      return Program.Elements.Modular_Types.Modular_Type_Access (Self);
   end To_Modular_Type;

   function To_Root_Type
    (Self : access Element'Class)
      return Program.Elements.Root_Types.Root_Type_Access is
   begin
      return Program.Elements.Root_Types.Root_Type_Access (Self);
   end To_Root_Type;

   function To_Floating_Point_Type
    (Self : access Element'Class)
      return Program.Elements.Floating_Point_Types
          .Floating_Point_Type_Access is
   begin
      return Program.Elements.Floating_Point_Types.Floating_Point_Type_Access
        (Self);
   end To_Floating_Point_Type;

   function To_Ordinary_Fixed_Point_Type
    (Self : access Element'Class)
      return Program.Elements.Ordinary_Fixed_Point_Types
          .Ordinary_Fixed_Point_Type_Access is
   begin
      return Program.Elements.Ordinary_Fixed_Point_Types
        .Ordinary_Fixed_Point_Type_Access
        (Self);
   end To_Ordinary_Fixed_Point_Type;

   function To_Decimal_Fixed_Point_Type
    (Self : access Element'Class)
      return Program.Elements.Decimal_Fixed_Point_Types
          .Decimal_Fixed_Point_Type_Access is
   begin
      return Program.Elements.Decimal_Fixed_Point_Types
        .Decimal_Fixed_Point_Type_Access
        (Self);
   end To_Decimal_Fixed_Point_Type;

   function To_Unconstrained_Array_Type
    (Self : access Element'Class)
      return Program.Elements.Unconstrained_Array_Types
          .Unconstrained_Array_Type_Access is
   begin
      return Program.Elements.Unconstrained_Array_Types
        .Unconstrained_Array_Type_Access
        (Self);
   end To_Unconstrained_Array_Type;

   function To_Constrained_Array_Type
    (Self : access Element'Class)
      return Program.Elements.Constrained_Array_Types
          .Constrained_Array_Type_Access is
   begin
      return Program.Elements.Constrained_Array_Types
        .Constrained_Array_Type_Access
        (Self);
   end To_Constrained_Array_Type;

   function To_Record_Type
    (Self : access Element'Class)
      return Program.Elements.Record_Types.Record_Type_Access is
   begin
      return Program.Elements.Record_Types.Record_Type_Access (Self);
   end To_Record_Type;

   function To_Interface_Type
    (Self : access Element'Class)
      return Program.Elements.Interface_Types.Interface_Type_Access is
   begin
      return Program.Elements.Interface_Types.Interface_Type_Access (Self);
   end To_Interface_Type;

   function To_Object_Access_Type
    (Self : access Element'Class)
      return Program.Elements.Object_Access_Types.Object_Access_Type_Access is
   begin
      return Program.Elements.Object_Access_Types.Object_Access_Type_Access
        (Self);
   end To_Object_Access_Type;

   function To_Procedure_Access_Type
    (Self : access Element'Class)
      return Program.Elements.Procedure_Access_Types
          .Procedure_Access_Type_Access is
   begin
      return Program.Elements.Procedure_Access_Types
        .Procedure_Access_Type_Access
        (Self);
   end To_Procedure_Access_Type;

   function To_Function_Access_Type
    (Self : access Element'Class)
      return Program.Elements.Function_Access_Types
          .Function_Access_Type_Access is
   begin
      return Program.Elements.Function_Access_Types.Function_Access_Type_Access
        (Self);
   end To_Function_Access_Type;

   function To_Formal_Private_Type_Definition
    (Self : access Element'Class)
      return Program.Elements.Formal_Private_Type_Definitions
          .Formal_Private_Type_Definition_Access is
   begin
      return Program.Elements.Formal_Private_Type_Definitions
        .Formal_Private_Type_Definition_Access
        (Self);
   end To_Formal_Private_Type_Definition;

   function To_Formal_Derived_Type_Definition
    (Self : access Element'Class)
      return Program.Elements.Formal_Derived_Type_Definitions
          .Formal_Derived_Type_Definition_Access is
   begin
      return Program.Elements.Formal_Derived_Type_Definitions
        .Formal_Derived_Type_Definition_Access
        (Self);
   end To_Formal_Derived_Type_Definition;

   function To_Formal_Discrete_Type_Definition
    (Self : access Element'Class)
      return Program.Elements.Formal_Discrete_Type_Definitions
          .Formal_Discrete_Type_Definition_Access is
   begin
      return Program.Elements.Formal_Discrete_Type_Definitions
        .Formal_Discrete_Type_Definition_Access
        (Self);
   end To_Formal_Discrete_Type_Definition;

   function To_Formal_Signed_Integer_Type_Definition
    (Self : access Element'Class)
      return Program.Elements.Formal_Signed_Integer_Type_Definitions
          .Formal_Signed_Integer_Type_Definition_Access is
   begin
      return Program.Elements.Formal_Signed_Integer_Type_Definitions
        .Formal_Signed_Integer_Type_Definition_Access
        (Self);
   end To_Formal_Signed_Integer_Type_Definition;

   function To_Formal_Modular_Type_Definition
    (Self : access Element'Class)
      return Program.Elements.Formal_Modular_Type_Definitions
          .Formal_Modular_Type_Definition_Access is
   begin
      return Program.Elements.Formal_Modular_Type_Definitions
        .Formal_Modular_Type_Definition_Access
        (Self);
   end To_Formal_Modular_Type_Definition;

   function To_Formal_Floating_Point_Definition
    (Self : access Element'Class)
      return Program.Elements.Formal_Floating_Point_Definitions
          .Formal_Floating_Point_Definition_Access is
   begin
      return Program.Elements.Formal_Floating_Point_Definitions
        .Formal_Floating_Point_Definition_Access
        (Self);
   end To_Formal_Floating_Point_Definition;

   function To_Formal_Ordinary_Fixed_Point_Definition
    (Self : access Element'Class)
      return Program.Elements.Formal_Ordinary_Fixed_Point_Definitions
          .Formal_Ordinary_Fixed_Point_Definition_Access is
   begin
      return Program.Elements.Formal_Ordinary_Fixed_Point_Definitions
        .Formal_Ordinary_Fixed_Point_Definition_Access
        (Self);
   end To_Formal_Ordinary_Fixed_Point_Definition;

   function To_Formal_Decimal_Fixed_Point_Definition
    (Self : access Element'Class)
      return Program.Elements.Formal_Decimal_Fixed_Point_Definitions
          .Formal_Decimal_Fixed_Point_Definition_Access is
   begin
      return Program.Elements.Formal_Decimal_Fixed_Point_Definitions
        .Formal_Decimal_Fixed_Point_Definition_Access
        (Self);
   end To_Formal_Decimal_Fixed_Point_Definition;

   function To_Formal_Access_Type
    (Self : access Element'Class)
      return Program.Elements.Formal_Access_Types.Formal_Access_Type_Access is
   begin
      return Program.Elements.Formal_Access_Types.Formal_Access_Type_Access
        (Self);
   end To_Formal_Access_Type;

   function To_Access_Type
    (Self : access Element'Class)
      return Program.Elements.Access_Types.Access_Type_Access is
   begin
      return Program.Elements.Access_Types.Access_Type_Access (Self);
   end To_Access_Type;

   function To_Range_Attribute_Reference
    (Self : access Element'Class)
      return Program.Elements.Range_Attribute_References
          .Range_Attribute_Reference_Access is
   begin
      return Program.Elements.Range_Attribute_References
        .Range_Attribute_Reference_Access
        (Self);
   end To_Range_Attribute_Reference;

   function To_Simple_Expression_Range
    (Self : access Element'Class)
      return Program.Elements.Simple_Expression_Ranges
          .Simple_Expression_Range_Access is
   begin
      return Program.Elements.Simple_Expression_Ranges
        .Simple_Expression_Range_Access
        (Self);
   end To_Simple_Expression_Range;

   function To_Digits_Constraint
    (Self : access Element'Class)
      return Program.Elements.Digits_Constraints.Digits_Constraint_Access is
   begin
      return Program.Elements.Digits_Constraints.Digits_Constraint_Access
        (Self);
   end To_Digits_Constraint;

   function To_Delta_Constraint
    (Self : access Element'Class)
      return Program.Elements.Delta_Constraints.Delta_Constraint_Access is
   begin
      return Program.Elements.Delta_Constraints.Delta_Constraint_Access (Self);
   end To_Delta_Constraint;

   function To_Index_Constraint
    (Self : access Element'Class)
      return Program.Elements.Index_Constraints.Index_Constraint_Access is
   begin
      return Program.Elements.Index_Constraints.Index_Constraint_Access (Self);
   end To_Index_Constraint;

   function To_Discriminant_Constraint
    (Self : access Element'Class)
      return Program.Elements.Discriminant_Constraints
          .Discriminant_Constraint_Access is
   begin
      return Program.Elements.Discriminant_Constraints
        .Discriminant_Constraint_Access
        (Self);
   end To_Discriminant_Constraint;

   function To_Attribute_Definition_Clause
    (Self : access Element'Class)
      return Program.Elements.Attribute_Definition_Clauses
          .Attribute_Definition_Clause_Access is
   begin
      return Program.Elements.Attribute_Definition_Clauses
        .Attribute_Definition_Clause_Access
        (Self);
   end To_Attribute_Definition_Clause;

   function To_Enumeration_Representation_Clause
    (Self : access Element'Class)
      return Program.Elements.Enumeration_Representation_Clauses
          .Enumeration_Representation_Clause_Access is
   begin
      return Program.Elements.Enumeration_Representation_Clauses
        .Enumeration_Representation_Clause_Access
        (Self);
   end To_Enumeration_Representation_Clause;

   function To_Record_Representation_Clause
    (Self : access Element'Class)
      return Program.Elements.Record_Representation_Clauses
          .Record_Representation_Clause_Access is
   begin
      return Program.Elements.Record_Representation_Clauses
        .Record_Representation_Clause_Access
        (Self);
   end To_Record_Representation_Clause;

   function To_At_Clause
    (Self : access Element'Class)
      return Program.Elements.At_Clauses.At_Clause_Access is
   begin
      return Program.Elements.At_Clauses.At_Clause_Access (Self);
   end To_At_Clause;

   function To_Exception_Handler
    (Self : access Element'Class)
      return Program.Elements.Exception_Handlers.Exception_Handler_Access is
   begin
      return Program.Elements.Exception_Handlers.Exception_Handler_Access
        (Self);
   end To_Exception_Handler;

end Program.Elements;
