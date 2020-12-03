--  Copyright (c) 2020 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Safe_Element_Visitors is

   procedure Visit
     (Self    : in out Safe_Element_Visitor'Class;
      Element : not null access Program.Elements.Element'Class) is
   begin
      Element.Visit (Self);
      pragma Assert (not Self.Failed);
   end Visit;

   overriding procedure Pragma_Element
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Pragmas.Pragma_Access) is
   begin
      Self.Failed := True;
   end Pragma_Element;

   overriding procedure Defining_Identifier
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access) is
   begin
      Self.Failed := True;
   end Defining_Identifier;

   overriding procedure Defining_Character_Literal
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Defining_Character_Literals
         .Defining_Character_Literal_Access) is
   begin
      Self.Failed := True;
   end Defining_Character_Literal;

   overriding procedure Defining_Operator_Symbol
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Defining_Operator_Symbols
         .Defining_Operator_Symbol_Access) is
   begin
      Self.Failed := True;
   end Defining_Operator_Symbol;

   overriding procedure Defining_Expanded_Name
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Defining_Expanded_Names
         .Defining_Expanded_Name_Access) is
   begin
      Self.Failed := True;
   end Defining_Expanded_Name;

   overriding procedure Type_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Type_Declarations
         .Type_Declaration_Access) is
   begin
      Self.Failed := True;
   end Type_Declaration;

   overriding procedure Task_Type_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Task_Type_Declarations
         .Task_Type_Declaration_Access) is
   begin
      Self.Failed := True;
   end Task_Type_Declaration;

   overriding procedure Protected_Type_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Protected_Type_Declarations
         .Protected_Type_Declaration_Access) is
   begin
      Self.Failed := True;
   end Protected_Type_Declaration;

   overriding procedure Subtype_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Subtype_Declarations
         .Subtype_Declaration_Access) is
   begin
      Self.Failed := True;
   end Subtype_Declaration;

   overriding procedure Object_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Object_Declarations
         .Object_Declaration_Access) is
   begin
      Self.Failed := True;
   end Object_Declaration;

   overriding procedure Single_Task_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Single_Task_Declarations
         .Single_Task_Declaration_Access) is
   begin
      Self.Failed := True;
   end Single_Task_Declaration;

   overriding procedure Single_Protected_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Single_Protected_Declarations
         .Single_Protected_Declaration_Access) is
   begin
      Self.Failed := True;
   end Single_Protected_Declaration;

   overriding procedure Number_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Number_Declarations
         .Number_Declaration_Access) is
   begin
      Self.Failed := True;
   end Number_Declaration;

   overriding procedure Enumeration_Literal_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Enumeration_Literal_Specifications
         .Enumeration_Literal_Specification_Access) is
   begin
      Self.Failed := True;
   end Enumeration_Literal_Specification;

   overriding procedure Discriminant_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Discriminant_Specifications
         .Discriminant_Specification_Access) is
   begin
      Self.Failed := True;
   end Discriminant_Specification;

   overriding procedure Component_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Component_Declarations
         .Component_Declaration_Access) is
   begin
      Self.Failed := True;
   end Component_Declaration;

   overriding procedure Loop_Parameter_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Loop_Parameter_Specifications
         .Loop_Parameter_Specification_Access) is
   begin
      Self.Failed := True;
   end Loop_Parameter_Specification;

   overriding procedure Generalized_Iterator_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Generalized_Iterator_Specifications
         .Generalized_Iterator_Specification_Access) is
   begin
      Self.Failed := True;
   end Generalized_Iterator_Specification;

   overriding procedure Element_Iterator_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Element_Iterator_Specifications
         .Element_Iterator_Specification_Access) is
   begin
      Self.Failed := True;
   end Element_Iterator_Specification;

   overriding procedure Procedure_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Procedure_Declarations
         .Procedure_Declaration_Access) is
   begin
      Self.Failed := True;
   end Procedure_Declaration;

   overriding procedure Function_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Function_Declarations
         .Function_Declaration_Access) is
   begin
      Self.Failed := True;
   end Function_Declaration;

   overriding procedure Parameter_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Parameter_Specifications
         .Parameter_Specification_Access) is
   begin
      Self.Failed := True;
   end Parameter_Specification;

   overriding procedure Procedure_Body_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Procedure_Body_Declarations
         .Procedure_Body_Declaration_Access) is
   begin
      Self.Failed := True;
   end Procedure_Body_Declaration;

   overriding procedure Function_Body_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Function_Body_Declarations
         .Function_Body_Declaration_Access) is
   begin
      Self.Failed := True;
   end Function_Body_Declaration;

   overriding procedure Return_Object_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Return_Object_Specifications
         .Return_Object_Specification_Access) is
   begin
      Self.Failed := True;
   end Return_Object_Specification;

   overriding procedure Package_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Package_Declarations
         .Package_Declaration_Access) is
   begin
      Self.Failed := True;
   end Package_Declaration;

   overriding procedure Package_Body_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Package_Body_Declarations
         .Package_Body_Declaration_Access) is
   begin
      Self.Failed := True;
   end Package_Body_Declaration;

   overriding procedure Object_Renaming_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Object_Renaming_Declarations
         .Object_Renaming_Declaration_Access) is
   begin
      Self.Failed := True;
   end Object_Renaming_Declaration;

   overriding procedure Exception_Renaming_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Exception_Renaming_Declarations
         .Exception_Renaming_Declaration_Access) is
   begin
      Self.Failed := True;
   end Exception_Renaming_Declaration;

   overriding procedure Procedure_Renaming_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Procedure_Renaming_Declarations
         .Procedure_Renaming_Declaration_Access) is
   begin
      Self.Failed := True;
   end Procedure_Renaming_Declaration;

   overriding procedure Function_Renaming_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Function_Renaming_Declarations
         .Function_Renaming_Declaration_Access) is
   begin
      Self.Failed := True;
   end Function_Renaming_Declaration;

   overriding procedure Package_Renaming_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Package_Renaming_Declarations
         .Package_Renaming_Declaration_Access) is
   begin
      Self.Failed := True;
   end Package_Renaming_Declaration;

   overriding procedure Generic_Package_Renaming_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Generic_Package_Renaming_Declarations
         .Generic_Package_Renaming_Declaration_Access) is
   begin
      Self.Failed := True;
   end Generic_Package_Renaming_Declaration;

   overriding procedure Generic_Procedure_Renaming_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements
         .Generic_Procedure_Renaming_Declarations
         .Generic_Procedure_Renaming_Declaration_Access) is
   begin
      Self.Failed := True;
   end Generic_Procedure_Renaming_Declaration;

   overriding procedure Generic_Function_Renaming_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Generic_Function_Renaming_Declarations
         .Generic_Function_Renaming_Declaration_Access) is
   begin
      Self.Failed := True;
   end Generic_Function_Renaming_Declaration;

   overriding procedure Task_Body_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Task_Body_Declarations
         .Task_Body_Declaration_Access) is
   begin
      Self.Failed := True;
   end Task_Body_Declaration;

   overriding procedure Protected_Body_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Protected_Body_Declarations
         .Protected_Body_Declaration_Access) is
   begin
      Self.Failed := True;
   end Protected_Body_Declaration;

   overriding procedure Entry_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Entry_Declarations
         .Entry_Declaration_Access) is
   begin
      Self.Failed := True;
   end Entry_Declaration;

   overriding procedure Entry_Body_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Entry_Body_Declarations
         .Entry_Body_Declaration_Access) is
   begin
      Self.Failed := True;
   end Entry_Body_Declaration;

   overriding procedure Entry_Index_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Entry_Index_Specifications
         .Entry_Index_Specification_Access) is
   begin
      Self.Failed := True;
   end Entry_Index_Specification;

   overriding procedure Procedure_Body_Stub
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Procedure_Body_Stubs
         .Procedure_Body_Stub_Access) is
   begin
      Self.Failed := True;
   end Procedure_Body_Stub;

   overriding procedure Function_Body_Stub
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Function_Body_Stubs
         .Function_Body_Stub_Access) is
   begin
      Self.Failed := True;
   end Function_Body_Stub;

   overriding procedure Package_Body_Stub
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Package_Body_Stubs
         .Package_Body_Stub_Access) is
   begin
      Self.Failed := True;
   end Package_Body_Stub;

   overriding procedure Task_Body_Stub
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Task_Body_Stubs
         .Task_Body_Stub_Access) is
   begin
      Self.Failed := True;
   end Task_Body_Stub;

   overriding procedure Protected_Body_Stub
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Protected_Body_Stubs
         .Protected_Body_Stub_Access) is
   begin
      Self.Failed := True;
   end Protected_Body_Stub;

   overriding procedure Exception_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Exception_Declarations
         .Exception_Declaration_Access) is
   begin
      Self.Failed := True;
   end Exception_Declaration;

   overriding procedure Choice_Parameter_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Choice_Parameter_Specifications
         .Choice_Parameter_Specification_Access) is
   begin
      Self.Failed := True;
   end Choice_Parameter_Specification;

   overriding procedure Generic_Package_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Generic_Package_Declarations
         .Generic_Package_Declaration_Access) is
   begin
      Self.Failed := True;
   end Generic_Package_Declaration;

   overriding procedure Generic_Procedure_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Generic_Procedure_Declarations
         .Generic_Procedure_Declaration_Access) is
   begin
      Self.Failed := True;
   end Generic_Procedure_Declaration;

   overriding procedure Generic_Function_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Generic_Function_Declarations
         .Generic_Function_Declaration_Access) is
   begin
      Self.Failed := True;
   end Generic_Function_Declaration;

   overriding procedure Package_Instantiation
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Package_Instantiations
         .Package_Instantiation_Access) is
   begin
      Self.Failed := True;
   end Package_Instantiation;

   overriding procedure Procedure_Instantiation
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Procedure_Instantiations
         .Procedure_Instantiation_Access) is
   begin
      Self.Failed := True;
   end Procedure_Instantiation;

   overriding procedure Function_Instantiation
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Function_Instantiations
         .Function_Instantiation_Access) is
   begin
      Self.Failed := True;
   end Function_Instantiation;

   overriding procedure Formal_Object_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Object_Declarations
         .Formal_Object_Declaration_Access) is
   begin
      Self.Failed := True;
   end Formal_Object_Declaration;

   overriding procedure Formal_Type_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Type_Declarations
         .Formal_Type_Declaration_Access) is
   begin
      Self.Failed := True;
   end Formal_Type_Declaration;

   overriding procedure Formal_Procedure_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Procedure_Declarations
         .Formal_Procedure_Declaration_Access) is
   begin
      Self.Failed := True;
   end Formal_Procedure_Declaration;

   overriding procedure Formal_Function_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Function_Declarations
         .Formal_Function_Declaration_Access) is
   begin
      Self.Failed := True;
   end Formal_Function_Declaration;

   overriding procedure Formal_Package_Declaration
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Package_Declarations
         .Formal_Package_Declaration_Access) is
   begin
      Self.Failed := True;
   end Formal_Package_Declaration;

   overriding procedure Subtype_Indication
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access) is
   begin
      Self.Failed := True;
   end Subtype_Indication;

   overriding procedure Component_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Component_Definitions
         .Component_Definition_Access) is
   begin
      Self.Failed := True;
   end Component_Definition;

   overriding procedure Discrete_Subtype_Indication
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Discrete_Subtype_Indications
         .Discrete_Subtype_Indication_Access) is
   begin
      Self.Failed := True;
   end Discrete_Subtype_Indication;

   overriding procedure Discrete_Range_Attribute_Reference
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Discrete_Range_Attribute_References
         .Discrete_Range_Attribute_Reference_Access) is
   begin
      Self.Failed := True;
   end Discrete_Range_Attribute_Reference;

   overriding procedure Discrete_Simple_Expression_Range
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Discrete_Simple_Expression_Ranges
         .Discrete_Simple_Expression_Range_Access) is
   begin
      Self.Failed := True;
   end Discrete_Simple_Expression_Range;

   overriding procedure Unknown_Discriminant_Part
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Unknown_Discriminant_Parts
         .Unknown_Discriminant_Part_Access) is
   begin
      Self.Failed := True;
   end Unknown_Discriminant_Part;

   overriding procedure Known_Discriminant_Part
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Known_Discriminant_Parts
         .Known_Discriminant_Part_Access) is
   begin
      Self.Failed := True;
   end Known_Discriminant_Part;

   overriding procedure Record_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Record_Definitions
         .Record_Definition_Access) is
   begin
      Self.Failed := True;
   end Record_Definition;

   overriding procedure Null_Component
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Null_Components
         .Null_Component_Access) is
   begin
      Self.Failed := True;
   end Null_Component;

   overriding procedure Variant_Part
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Variant_Parts.Variant_Part_Access) is
   begin
      Self.Failed := True;
   end Variant_Part;

   overriding procedure Variant
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Variants.Variant_Access) is
   begin
      Self.Failed := True;
   end Variant;

   overriding procedure Others_Choice
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Others_Choices.Others_Choice_Access) is
   begin
      Self.Failed := True;
   end Others_Choice;

   overriding procedure Anonymous_Access_To_Object
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Anonymous_Access_To_Objects
         .Anonymous_Access_To_Object_Access) is
   begin
      Self.Failed := True;
   end Anonymous_Access_To_Object;

   overriding procedure Anonymous_Access_To_Procedure
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Anonymous_Access_To_Procedures
         .Anonymous_Access_To_Procedure_Access) is
   begin
      Self.Failed := True;
   end Anonymous_Access_To_Procedure;

   overriding procedure Anonymous_Access_To_Function
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Anonymous_Access_To_Functions
         .Anonymous_Access_To_Function_Access) is
   begin
      Self.Failed := True;
   end Anonymous_Access_To_Function;

   overriding procedure Private_Type_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Private_Type_Definitions
         .Private_Type_Definition_Access) is
   begin
      Self.Failed := True;
   end Private_Type_Definition;

   overriding procedure Private_Extension_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Private_Extension_Definitions
         .Private_Extension_Definition_Access) is
   begin
      Self.Failed := True;
   end Private_Extension_Definition;

   overriding procedure Incomplete_Type_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Incomplete_Type_Definitions
         .Incomplete_Type_Definition_Access) is
   begin
      Self.Failed := True;
   end Incomplete_Type_Definition;

   overriding procedure Task_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Task_Definitions
         .Task_Definition_Access) is
   begin
      Self.Failed := True;
   end Task_Definition;

   overriding procedure Protected_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Protected_Definitions
         .Protected_Definition_Access) is
   begin
      Self.Failed := True;
   end Protected_Definition;

   overriding procedure Aspect_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Access) is
   begin
      Self.Failed := True;
   end Aspect_Specification;

   overriding procedure Real_Range_Specification
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access) is
   begin
      Self.Failed := True;
   end Real_Range_Specification;

   overriding procedure Numeric_Literal
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Numeric_Literals
         .Numeric_Literal_Access) is
   begin
      Self.Failed := True;
   end Numeric_Literal;

   overriding procedure String_Literal
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.String_Literals
         .String_Literal_Access) is
   begin
      Self.Failed := True;
   end String_Literal;

   overriding procedure Identifier
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Identifiers.Identifier_Access) is
   begin
      Self.Failed := True;
   end Identifier;

   overriding procedure Operator_Symbol
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Operator_Symbols
         .Operator_Symbol_Access) is
   begin
      Self.Failed := True;
   end Operator_Symbol;

   overriding procedure Character_Literal
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Character_Literals
         .Character_Literal_Access) is
   begin
      Self.Failed := True;
   end Character_Literal;

   overriding procedure Explicit_Dereference
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Explicit_Dereferences
         .Explicit_Dereference_Access) is
   begin
      Self.Failed := True;
   end Explicit_Dereference;

   overriding procedure Infix_Operator
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Infix_Operators
         .Infix_Operator_Access) is
   begin
      Self.Failed := True;
   end Infix_Operator;

   overriding procedure Function_Call
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Function_Calls.Function_Call_Access) is
   begin
      Self.Failed := True;
   end Function_Call;

   overriding procedure Indexed_Component
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Indexed_Components
         .Indexed_Component_Access) is
   begin
      Self.Failed := True;
   end Indexed_Component;

   overriding procedure Slice
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Slices.Slice_Access) is
   begin
      Self.Failed := True;
   end Slice;

   overriding procedure Selected_Component
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Selected_Components
         .Selected_Component_Access) is
   begin
      Self.Failed := True;
   end Selected_Component;

   overriding procedure Attribute_Reference
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Attribute_References
         .Attribute_Reference_Access) is
   begin
      Self.Failed := True;
   end Attribute_Reference;

   overriding procedure Record_Aggregate
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Record_Aggregates
         .Record_Aggregate_Access) is
   begin
      Self.Failed := True;
   end Record_Aggregate;

   overriding procedure Extension_Aggregate
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Extension_Aggregates
         .Extension_Aggregate_Access) is
   begin
      Self.Failed := True;
   end Extension_Aggregate;

   overriding procedure Array_Aggregate
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Array_Aggregates
         .Array_Aggregate_Access) is
   begin
      Self.Failed := True;
   end Array_Aggregate;

   overriding procedure Short_Circuit_Operation
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Short_Circuit_Operations
         .Short_Circuit_Operation_Access) is
   begin
      Self.Failed := True;
   end Short_Circuit_Operation;

   overriding procedure Membership_Test
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Membership_Tests
         .Membership_Test_Access) is
   begin
      Self.Failed := True;
   end Membership_Test;

   overriding procedure Null_Literal
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Null_Literals.Null_Literal_Access) is
   begin
      Self.Failed := True;
   end Null_Literal;

   overriding procedure Parenthesized_Expression
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Parenthesized_Expressions
         .Parenthesized_Expression_Access) is
   begin
      Self.Failed := True;
   end Parenthesized_Expression;

   overriding procedure Raise_Expression
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Raise_Expressions
         .Raise_Expression_Access) is
   begin
      Self.Failed := True;
   end Raise_Expression;

   overriding procedure Type_Conversion
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Type_Conversions
         .Type_Conversion_Access) is
   begin
      Self.Failed := True;
   end Type_Conversion;

   overriding procedure Qualified_Expression
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Qualified_Expressions
         .Qualified_Expression_Access) is
   begin
      Self.Failed := True;
   end Qualified_Expression;

   overriding procedure Allocator
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Allocators.Allocator_Access) is
   begin
      Self.Failed := True;
   end Allocator;

   overriding procedure Case_Expression
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Case_Expressions
         .Case_Expression_Access) is
   begin
      Self.Failed := True;
   end Case_Expression;

   overriding procedure If_Expression
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.If_Expressions.If_Expression_Access) is
   begin
      Self.Failed := True;
   end If_Expression;

   overriding procedure Quantified_Expression
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Quantified_Expressions
         .Quantified_Expression_Access) is
   begin
      Self.Failed := True;
   end Quantified_Expression;

   overriding procedure Discriminant_Association
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Discriminant_Associations
         .Discriminant_Association_Access) is
   begin
      Self.Failed := True;
   end Discriminant_Association;

   overriding procedure Record_Component_Association
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Record_Component_Associations
         .Record_Component_Association_Access) is
   begin
      Self.Failed := True;
   end Record_Component_Association;

   overriding procedure Array_Component_Association
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Array_Component_Associations
         .Array_Component_Association_Access) is
   begin
      Self.Failed := True;
   end Array_Component_Association;

   overriding procedure Parameter_Association
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Parameter_Associations
         .Parameter_Association_Access) is
   begin
      Self.Failed := True;
   end Parameter_Association;

   overriding procedure Formal_Package_Association
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Package_Associations
         .Formal_Package_Association_Access) is
   begin
      Self.Failed := True;
   end Formal_Package_Association;

   overriding procedure Null_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Null_Statements
         .Null_Statement_Access) is
   begin
      Self.Failed := True;
   end Null_Statement;

   overriding procedure Assignment_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Assignment_Statements
         .Assignment_Statement_Access) is
   begin
      Self.Failed := True;
   end Assignment_Statement;

   overriding procedure If_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.If_Statements.If_Statement_Access) is
   begin
      Self.Failed := True;
   end If_Statement;

   overriding procedure Case_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Case_Statements
         .Case_Statement_Access) is
   begin
      Self.Failed := True;
   end Case_Statement;

   overriding procedure Loop_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Loop_Statements
         .Loop_Statement_Access) is
   begin
      Self.Failed := True;
   end Loop_Statement;

   overriding procedure While_Loop_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.While_Loop_Statements
         .While_Loop_Statement_Access) is
   begin
      Self.Failed := True;
   end While_Loop_Statement;

   overriding procedure For_Loop_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.For_Loop_Statements
         .For_Loop_Statement_Access) is
   begin
      Self.Failed := True;
   end For_Loop_Statement;

   overriding procedure Block_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Block_Statements
         .Block_Statement_Access) is
   begin
      Self.Failed := True;
   end Block_Statement;

   overriding procedure Exit_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Exit_Statements
         .Exit_Statement_Access) is
   begin
      Self.Failed := True;
   end Exit_Statement;

   overriding procedure Goto_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Goto_Statements
         .Goto_Statement_Access) is
   begin
      Self.Failed := True;
   end Goto_Statement;

   overriding procedure Call_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Call_Statements
         .Call_Statement_Access) is
   begin
      Self.Failed := True;
   end Call_Statement;

   overriding procedure Simple_Return_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Simple_Return_Statements
         .Simple_Return_Statement_Access) is
   begin
      Self.Failed := True;
   end Simple_Return_Statement;

   overriding procedure Extended_Return_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Extended_Return_Statements
         .Extended_Return_Statement_Access) is
   begin
      Self.Failed := True;
   end Extended_Return_Statement;

   overriding procedure Accept_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Accept_Statements
         .Accept_Statement_Access) is
   begin
      Self.Failed := True;
   end Accept_Statement;

   overriding procedure Requeue_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Requeue_Statements
         .Requeue_Statement_Access) is
   begin
      Self.Failed := True;
   end Requeue_Statement;

   overriding procedure Delay_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Delay_Statements
         .Delay_Statement_Access) is
   begin
      Self.Failed := True;
   end Delay_Statement;

   overriding procedure Terminate_Alternative_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Terminate_Alternative_Statements
         .Terminate_Alternative_Statement_Access) is
   begin
      Self.Failed := True;
   end Terminate_Alternative_Statement;

   overriding procedure Select_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Select_Statements
         .Select_Statement_Access) is
   begin
      Self.Failed := True;
   end Select_Statement;

   overriding procedure Abort_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Abort_Statements
         .Abort_Statement_Access) is
   begin
      Self.Failed := True;
   end Abort_Statement;

   overriding procedure Raise_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Raise_Statements
         .Raise_Statement_Access) is
   begin
      Self.Failed := True;
   end Raise_Statement;

   overriding procedure Code_Statement
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Code_Statements
         .Code_Statement_Access) is
   begin
      Self.Failed := True;
   end Code_Statement;

   overriding procedure Elsif_Path
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Elsif_Paths.Elsif_Path_Access) is
   begin
      Self.Failed := True;
   end Elsif_Path;

   overriding procedure Case_Path
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Case_Paths.Case_Path_Access) is
   begin
      Self.Failed := True;
   end Case_Path;

   overriding procedure Select_Path
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Select_Paths.Select_Path_Access) is
   begin
      Self.Failed := True;
   end Select_Path;

   overriding procedure Case_Expression_Path
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Case_Expression_Paths
         .Case_Expression_Path_Access) is
   begin
      Self.Failed := True;
   end Case_Expression_Path;

   overriding procedure Elsif_Expression_Path
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Elsif_Expression_Paths
         .Elsif_Expression_Path_Access) is
   begin
      Self.Failed := True;
   end Elsif_Expression_Path;

   overriding procedure Use_Clause
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Use_Clauses.Use_Clause_Access) is
   begin
      Self.Failed := True;
   end Use_Clause;

   overriding procedure With_Clause
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.With_Clauses.With_Clause_Access) is
   begin
      Self.Failed := True;
   end With_Clause;

   overriding procedure Component_Clause
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Component_Clauses
         .Component_Clause_Access) is
   begin
      Self.Failed := True;
   end Component_Clause;

   overriding procedure Derived_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Derived_Types.Derived_Type_Access) is
   begin
      Self.Failed := True;
   end Derived_Type;

   overriding procedure Derived_Record_Extension
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Derived_Record_Extensions
         .Derived_Record_Extension_Access) is
   begin
      Self.Failed := True;
   end Derived_Record_Extension;

   overriding procedure Enumeration_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Enumeration_Types
         .Enumeration_Type_Access) is
   begin
      Self.Failed := True;
   end Enumeration_Type;

   overriding procedure Signed_Integer_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Signed_Integer_Types
         .Signed_Integer_Type_Access) is
   begin
      Self.Failed := True;
   end Signed_Integer_Type;

   overriding procedure Modular_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Modular_Types.Modular_Type_Access) is
   begin
      Self.Failed := True;
   end Modular_Type;

   overriding procedure Root_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Root_Types.Root_Type_Access) is
   begin
      Self.Failed := True;
   end Root_Type;

   overriding procedure Floating_Point_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Floating_Point_Types
         .Floating_Point_Type_Access) is
   begin
      Self.Failed := True;
   end Floating_Point_Type;

   overriding procedure Ordinary_Fixed_Point_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Ordinary_Fixed_Point_Types
         .Ordinary_Fixed_Point_Type_Access) is
   begin
      Self.Failed := True;
   end Ordinary_Fixed_Point_Type;

   overriding procedure Decimal_Fixed_Point_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Decimal_Fixed_Point_Types
         .Decimal_Fixed_Point_Type_Access) is
   begin
      Self.Failed := True;
   end Decimal_Fixed_Point_Type;

   overriding procedure Unconstrained_Array_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Unconstrained_Array_Types
         .Unconstrained_Array_Type_Access) is
   begin
      Self.Failed := True;
   end Unconstrained_Array_Type;

   overriding procedure Constrained_Array_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Constrained_Array_Types
         .Constrained_Array_Type_Access) is
   begin
      Self.Failed := True;
   end Constrained_Array_Type;

   overriding procedure Record_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Record_Types.Record_Type_Access) is
   begin
      Self.Failed := True;
   end Record_Type;

   overriding procedure Interface_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Interface_Types
         .Interface_Type_Access) is
   begin
      Self.Failed := True;
   end Interface_Type;

   overriding procedure Object_Access_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Object_Access_Types
         .Object_Access_Type_Access) is
   begin
      Self.Failed := True;
   end Object_Access_Type;

   overriding procedure Procedure_Access_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Procedure_Access_Types
         .Procedure_Access_Type_Access) is
   begin
      Self.Failed := True;
   end Procedure_Access_Type;

   overriding procedure Function_Access_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Function_Access_Types
         .Function_Access_Type_Access) is
   begin
      Self.Failed := True;
   end Function_Access_Type;

   overriding procedure Formal_Private_Type_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Private_Type_Definitions
         .Formal_Private_Type_Definition_Access) is
   begin
      Self.Failed := True;
   end Formal_Private_Type_Definition;

   overriding procedure Formal_Derived_Type_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Derived_Type_Definitions
         .Formal_Derived_Type_Definition_Access) is
   begin
      Self.Failed := True;
   end Formal_Derived_Type_Definition;

   overriding procedure Formal_Discrete_Type_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Discrete_Type_Definitions
         .Formal_Discrete_Type_Definition_Access) is
   begin
      Self.Failed := True;
   end Formal_Discrete_Type_Definition;

   overriding procedure Formal_Signed_Integer_Type_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Signed_Integer_Type_Definitions
         .Formal_Signed_Integer_Type_Definition_Access) is
   begin
      Self.Failed := True;
   end Formal_Signed_Integer_Type_Definition;

   overriding procedure Formal_Modular_Type_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Modular_Type_Definitions
         .Formal_Modular_Type_Definition_Access) is
   begin
      Self.Failed := True;
   end Formal_Modular_Type_Definition;

   overriding procedure Formal_Floating_Point_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Floating_Point_Definitions
         .Formal_Floating_Point_Definition_Access) is
   begin
      Self.Failed := True;
   end Formal_Floating_Point_Definition;

   overriding procedure Formal_Ordinary_Fixed_Point_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements
         .Formal_Ordinary_Fixed_Point_Definitions
         .Formal_Ordinary_Fixed_Point_Definition_Access) is
   begin
      Self.Failed := True;
   end Formal_Ordinary_Fixed_Point_Definition;

   overriding procedure Formal_Decimal_Fixed_Point_Definition
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Decimal_Fixed_Point_Definitions
         .Formal_Decimal_Fixed_Point_Definition_Access) is
   begin
      Self.Failed := True;
   end Formal_Decimal_Fixed_Point_Definition;

   overriding procedure Formal_Unconstrained_Array_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Unconstrained_Array_Types
         .Formal_Unconstrained_Array_Type_Access) is
   begin
      Self.Failed := True;
   end Formal_Unconstrained_Array_Type;

   overriding procedure Formal_Constrained_Array_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Constrained_Array_Types
         .Formal_Constrained_Array_Type_Access) is
   begin
      Self.Failed := True;
   end Formal_Constrained_Array_Type;

   overriding procedure Formal_Object_Access_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Object_Access_Types
         .Formal_Object_Access_Type_Access) is
   begin
      Self.Failed := True;
   end Formal_Object_Access_Type;

   overriding procedure Formal_Procedure_Access_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Procedure_Access_Types
         .Formal_Procedure_Access_Type_Access) is
   begin
      Self.Failed := True;
   end Formal_Procedure_Access_Type;

   overriding procedure Formal_Function_Access_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Function_Access_Types
         .Formal_Function_Access_Type_Access) is
   begin
      Self.Failed := True;
   end Formal_Function_Access_Type;

   overriding procedure Formal_Interface_Type
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Formal_Interface_Types
         .Formal_Interface_Type_Access) is
   begin
      Self.Failed := True;
   end Formal_Interface_Type;

   overriding procedure Range_Attribute_Reference
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Range_Attribute_References
         .Range_Attribute_Reference_Access) is
   begin
      Self.Failed := True;
   end Range_Attribute_Reference;

   overriding procedure Simple_Expression_Range
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Simple_Expression_Ranges
         .Simple_Expression_Range_Access) is
   begin
      Self.Failed := True;
   end Simple_Expression_Range;

   overriding procedure Digits_Constraint
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Digits_Constraints
         .Digits_Constraint_Access) is
   begin
      Self.Failed := True;
   end Digits_Constraint;

   overriding procedure Delta_Constraint
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Delta_Constraints
         .Delta_Constraint_Access) is
   begin
      Self.Failed := True;
   end Delta_Constraint;

   overriding procedure Index_Constraint
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Index_Constraints
         .Index_Constraint_Access) is
   begin
      Self.Failed := True;
   end Index_Constraint;

   overriding procedure Discriminant_Constraint
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Discriminant_Constraints
         .Discriminant_Constraint_Access) is
   begin
      Self.Failed := True;
   end Discriminant_Constraint;

   overriding procedure Attribute_Definition_Clause
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Attribute_Definition_Clauses
         .Attribute_Definition_Clause_Access) is
   begin
      Self.Failed := True;
   end Attribute_Definition_Clause;

   overriding procedure Enumeration_Representation_Clause
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Enumeration_Representation_Clauses
         .Enumeration_Representation_Clause_Access) is
   begin
      Self.Failed := True;
   end Enumeration_Representation_Clause;

   overriding procedure Record_Representation_Clause
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Record_Representation_Clauses
         .Record_Representation_Clause_Access) is
   begin
      Self.Failed := True;
   end Record_Representation_Clause;

   overriding procedure At_Clause
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.At_Clauses.At_Clause_Access) is
   begin
      Self.Failed := True;
   end At_Clause;

   overriding procedure Exception_Handler
    (Self   : in out Safe_Element_Visitor;
     Ignore : not null Program.Elements.Exception_Handlers
         .Exception_Handler_Access) is
   begin
      Self.Failed := True;
   end Exception_Handler;

end Program.Safe_Element_Visitors;
