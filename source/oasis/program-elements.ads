--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

limited with Program.Elements.Pragmas;
limited with Program.Elements.Defining_Names;
limited with Program.Elements.Defining_Identifiers;
limited with Program.Elements.Defining_Character_Literals;
limited with Program.Elements.Defining_Operator_Symbols;
limited with Program.Elements.Defining_Expanded_Names;
limited with Program.Elements.Declarations;
limited with Program.Elements.Type_Declarations;
limited with Program.Elements.Task_Type_Declarations;
limited with Program.Elements.Protected_Type_Declarations;
limited with Program.Elements.Subtype_Declarations;
limited with Program.Elements.Object_Declarations;
limited with Program.Elements.Single_Task_Declarations;
limited with Program.Elements.Single_Protected_Declarations;
limited with Program.Elements.Number_Declarations;
limited with Program.Elements.Enumeration_Literal_Specifications;
limited with Program.Elements.Discriminant_Specifications;
limited with Program.Elements.Component_Declarations;
limited with Program.Elements.Loop_Parameter_Specifications;
limited with Program.Elements.Generalized_Iterator_Specifications;
limited with Program.Elements.Element_Iterator_Specifications;
limited with Program.Elements.Procedure_Declarations;
limited with Program.Elements.Function_Declarations;
limited with Program.Elements.Parameter_Specifications;
limited with Program.Elements.Procedure_Body_Declarations;
limited with Program.Elements.Function_Body_Declarations;
limited with Program.Elements.Return_Object_Specifications;
limited with Program.Elements.Package_Declarations;
limited with Program.Elements.Package_Body_Declarations;
limited with Program.Elements.Object_Renaming_Declarations;
limited with Program.Elements.Exception_Renaming_Declarations;
limited with Program.Elements.Procedure_Renaming_Declarations;
limited with Program.Elements.Function_Renaming_Declarations;
limited with Program.Elements.Package_Renaming_Declarations;
limited with Program.Elements.Generic_Package_Renaming_Declarations;
limited with Program.Elements.Generic_Procedure_Renaming_Declarations;
limited with Program.Elements.Generic_Function_Renaming_Declarations;
limited with Program.Elements.Task_Body_Declarations;
limited with Program.Elements.Protected_Body_Declarations;
limited with Program.Elements.Entry_Declarations;
limited with Program.Elements.Entry_Body_Declarations;
limited with Program.Elements.Entry_Index_Specifications;
limited with Program.Elements.Procedure_Body_Stubs;
limited with Program.Elements.Function_Body_Stubs;
limited with Program.Elements.Package_Body_Stubs;
limited with Program.Elements.Task_Body_Stubs;
limited with Program.Elements.Protected_Body_Stubs;
limited with Program.Elements.Exception_Declarations;
limited with Program.Elements.Choice_Parameter_Specifications;
limited with Program.Elements.Generic_Package_Declarations;
limited with Program.Elements.Generic_Procedure_Declarations;
limited with Program.Elements.Generic_Function_Declarations;
limited with Program.Elements.Package_Instantiations;
limited with Program.Elements.Procedure_Instantiations;
limited with Program.Elements.Function_Instantiations;
limited with Program.Elements.Formal_Object_Declarations;
limited with Program.Elements.Formal_Type_Declarations;
limited with Program.Elements.Formal_Procedure_Declarations;
limited with Program.Elements.Formal_Function_Declarations;
limited with Program.Elements.Formal_Package_Declarations;
limited with Program.Elements.Definitions;
limited with Program.Elements.Type_Definitions;
limited with Program.Elements.Subtype_Indications;
limited with Program.Elements.Constraints;
limited with Program.Elements.Component_Definitions;
limited with Program.Elements.Discrete_Ranges;
limited with Program.Elements.Discrete_Subtype_Indications;
limited with Program.Elements.Discrete_Range_Attribute_References;
limited with Program.Elements.Discrete_Simple_Expression_Ranges;
limited with Program.Elements.Unknown_Discriminant_Parts;
limited with Program.Elements.Known_Discriminant_Parts;
limited with Program.Elements.Record_Definitions;
limited with Program.Elements.Null_Components;
limited with Program.Elements.Variant_Parts;
limited with Program.Elements.Variants;
limited with Program.Elements.Others_Choices;
limited with Program.Elements.Anonymous_Access_Definitions;
limited with Program.Elements.Anonymous_Access_To_Objects;
limited with Program.Elements.Anonymous_Access_To_Procedures;
limited with Program.Elements.Anonymous_Access_To_Functions;
limited with Program.Elements.Private_Type_Definitions;
limited with Program.Elements.Private_Extension_Definitions;
limited with Program.Elements.Incomplete_Type_Definitions;
limited with Program.Elements.Task_Definitions;
limited with Program.Elements.Protected_Definitions;
limited with Program.Elements.Formal_Type_Definitions;
limited with Program.Elements.Aspect_Specifications;
limited with Program.Elements.Real_Range_Specifications;
limited with Program.Elements.Expressions;
limited with Program.Elements.Numeric_Literals;
limited with Program.Elements.String_Literals;
limited with Program.Elements.Identifiers;
limited with Program.Elements.Operator_Symbols;
limited with Program.Elements.Character_Literals;
limited with Program.Elements.Explicit_Dereferences;
limited with Program.Elements.Function_Calls;
limited with Program.Elements.Indexed_Components;
limited with Program.Elements.Slices;
limited with Program.Elements.Selected_Components;
limited with Program.Elements.Attribute_References;
limited with Program.Elements.Record_Aggregates;
limited with Program.Elements.Extension_Aggregates;
limited with Program.Elements.Array_Aggregates;
limited with Program.Elements.Short_Circuit_Operations;
limited with Program.Elements.Membership_Tests;
limited with Program.Elements.Null_Literals;
limited with Program.Elements.Parenthesized_Expressions;
limited with Program.Elements.Raise_Expressions;
limited with Program.Elements.Type_Conversions;
limited with Program.Elements.Qualified_Expressions;
limited with Program.Elements.Allocators;
limited with Program.Elements.Case_Expressions;
limited with Program.Elements.If_Expressions;
limited with Program.Elements.Quantified_Expressions;
limited with Program.Elements.Associations;
limited with Program.Elements.Discriminant_Associations;
limited with Program.Elements.Record_Component_Associations;
limited with Program.Elements.Array_Component_Associations;
limited with Program.Elements.Parameter_Associations;
limited with Program.Elements.Formal_Package_Associations;
limited with Program.Elements.Statements;
limited with Program.Elements.Null_Statements;
limited with Program.Elements.Assignment_Statements;
limited with Program.Elements.If_Statements;
limited with Program.Elements.Case_Statements;
limited with Program.Elements.Loop_Statements;
limited with Program.Elements.While_Loop_Statements;
limited with Program.Elements.For_Loop_Statements;
limited with Program.Elements.Block_Statements;
limited with Program.Elements.Exit_Statements;
limited with Program.Elements.Goto_Statements;
limited with Program.Elements.Call_Statements;
limited with Program.Elements.Simple_Return_Statements;
limited with Program.Elements.Extended_Return_Statements;
limited with Program.Elements.Accept_Statements;
limited with Program.Elements.Requeue_Statements;
limited with Program.Elements.Delay_Statements;
limited with Program.Elements.Terminate_Alternative_Statements;
limited with Program.Elements.Select_Statements;
limited with Program.Elements.Abort_Statements;
limited with Program.Elements.Raise_Statements;
limited with Program.Elements.Code_Statements;
limited with Program.Elements.Paths;
limited with Program.Elements.Elsif_Paths;
limited with Program.Elements.Case_Paths;
limited with Program.Elements.Select_Paths;
limited with Program.Elements.Case_Expression_Paths;
limited with Program.Elements.Elsif_Expression_Paths;
limited with Program.Elements.Clauses;
limited with Program.Elements.Use_Clauses;
limited with Program.Elements.With_Clauses;
limited with Program.Elements.Representation_Clauses;
limited with Program.Elements.Component_Clauses;
limited with Program.Elements.Derived_Types;
limited with Program.Elements.Derived_Record_Extensions;
limited with Program.Elements.Enumeration_Types;
limited with Program.Elements.Signed_Integer_Types;
limited with Program.Elements.Modular_Types;
limited with Program.Elements.Root_Types;
limited with Program.Elements.Floating_Point_Types;
limited with Program.Elements.Ordinary_Fixed_Point_Types;
limited with Program.Elements.Decimal_Fixed_Point_Types;
limited with Program.Elements.Unconstrained_Array_Types;
limited with Program.Elements.Constrained_Array_Types;
limited with Program.Elements.Record_Types;
limited with Program.Elements.Interface_Types;
limited with Program.Elements.Object_Access_Types;
limited with Program.Elements.Procedure_Access_Types;
limited with Program.Elements.Function_Access_Types;
limited with Program.Elements.Formal_Private_Type_Definitions;
limited with Program.Elements.Formal_Derived_Type_Definitions;
limited with Program.Elements.Formal_Discrete_Type_Definitions;
limited with Program.Elements.Formal_Signed_Integer_Type_Definitions;
limited with Program.Elements.Formal_Modular_Type_Definitions;
limited with Program.Elements.Formal_Floating_Point_Definitions;
limited with Program.Elements.Formal_Ordinary_Fixed_Point_Definitions;
limited with Program.Elements.Formal_Decimal_Fixed_Point_Definitions;
limited with Program.Elements.Formal_Unconstrained_Array_Types;
limited with Program.Elements.Formal_Constrained_Array_Types;
limited with Program.Elements.Formal_Access_Types;
limited with Program.Elements.Formal_Object_Access_Types;
limited with Program.Elements.Formal_Procedure_Access_Types;
limited with Program.Elements.Formal_Function_Access_Types;
limited with Program.Elements.Formal_Interface_Types;
limited with Program.Elements.Access_Types;
limited with Program.Elements.Range_Attribute_References;
limited with Program.Elements.Simple_Expression_Ranges;
limited with Program.Elements.Digits_Constraints;
limited with Program.Elements.Delta_Constraints;
limited with Program.Elements.Index_Constraints;
limited with Program.Elements.Discriminant_Constraints;
limited with Program.Elements.Attribute_Definition_Clauses;
limited with Program.Elements.Enumeration_Representation_Clauses;
limited with Program.Elements.Record_Representation_Clauses;
limited with Program.Elements.At_Clauses;
limited with Program.Elements.Exception_Handlers;
limited with Program.Element_Visitors;
with Program.Element_Iterators;

package Program.Elements is

   pragma Pure (Program.Elements);

   type Element is limited interface;

   type Element_Access is access all Element'Class with Storage_Size => 0;

   function Assigned (Self : access Element'Class) return Boolean
     is (Self /= null);

   not overriding function Is_Pragma (Self : Element) return Boolean
     is abstract;

   not overriding function Is_Defining_Name (Self : Element) return Boolean
     is abstract;

   not overriding function Is_Defining_Identifier
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Defining_Identifier'Result then Self.Is_Defining_Name);

   not overriding function Is_Defining_Character_Literal
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Defining_Character_Literal'Result then Self.Is_Defining_Name);

   not overriding function Is_Defining_Operator_Symbol
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Defining_Operator_Symbol'Result then Self.Is_Defining_Name);

   not overriding function Is_Defining_Expanded_Name
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Defining_Expanded_Name'Result then Self.Is_Defining_Name);

   not overriding function Is_Declaration (Self : Element) return Boolean
     is abstract;

   not overriding function Is_Type_Declaration (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_Type_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Task_Type_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Task_Type_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Protected_Type_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Protected_Type_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Subtype_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Subtype_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Object_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Object_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Single_Task_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Single_Task_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Single_Protected_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Single_Protected_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Number_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Number_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Enumeration_Literal_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Enumeration_Literal_Specification'Result
          then Self.Is_Declaration);

   not overriding function Is_Discriminant_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Discriminant_Specification'Result then Self.Is_Declaration);

   not overriding function Is_Component_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Component_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Loop_Parameter_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Loop_Parameter_Specification'Result then Self.Is_Declaration);

   not overriding function Is_Generalized_Iterator_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Generalized_Iterator_Specification'Result
          then Self.Is_Declaration);

   not overriding function Is_Element_Iterator_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Element_Iterator_Specification'Result then Self.Is_Declaration);

   not overriding function Is_Procedure_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Procedure_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Function_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Function_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Parameter_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Parameter_Specification'Result then Self.Is_Declaration);

   not overriding function Is_Procedure_Body_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Procedure_Body_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Function_Body_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Function_Body_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Return_Object_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Return_Object_Specification'Result then Self.Is_Declaration);

   not overriding function Is_Package_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Package_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Package_Body_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Package_Body_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Object_Renaming_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Object_Renaming_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Exception_Renaming_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Exception_Renaming_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Procedure_Renaming_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Procedure_Renaming_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Function_Renaming_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Function_Renaming_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Package_Renaming_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Package_Renaming_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Generic_Package_Renaming_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Generic_Package_Renaming_Declaration'Result
          then Self.Is_Declaration);

   not overriding function Is_Generic_Procedure_Renaming_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Generic_Procedure_Renaming_Declaration'Result
          then Self.Is_Declaration);

   not overriding function Is_Generic_Function_Renaming_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Generic_Function_Renaming_Declaration'Result
          then Self.Is_Declaration);

   not overriding function Is_Task_Body_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Task_Body_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Protected_Body_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Protected_Body_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Entry_Declaration (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_Entry_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Entry_Body_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Entry_Body_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Entry_Index_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Entry_Index_Specification'Result then Self.Is_Declaration);

   not overriding function Is_Procedure_Body_Stub
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Procedure_Body_Stub'Result then Self.Is_Declaration);

   not overriding function Is_Function_Body_Stub
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Function_Body_Stub'Result then Self.Is_Declaration);

   not overriding function Is_Package_Body_Stub (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_Package_Body_Stub'Result then Self.Is_Declaration);

   not overriding function Is_Task_Body_Stub (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Task_Body_Stub'Result then Self.Is_Declaration);

   not overriding function Is_Protected_Body_Stub
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Protected_Body_Stub'Result then Self.Is_Declaration);

   not overriding function Is_Exception_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Exception_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Choice_Parameter_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Choice_Parameter_Specification'Result then Self.Is_Declaration);

   not overriding function Is_Generic_Package_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Generic_Package_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Generic_Procedure_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Generic_Procedure_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Generic_Function_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Generic_Function_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Package_Instantiation
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Package_Instantiation'Result then Self.Is_Declaration);

   not overriding function Is_Procedure_Instantiation
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Procedure_Instantiation'Result then Self.Is_Declaration);

   not overriding function Is_Function_Instantiation
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Function_Instantiation'Result then Self.Is_Declaration);

   not overriding function Is_Formal_Object_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Object_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Formal_Type_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Type_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Formal_Procedure_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Procedure_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Formal_Function_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Function_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Formal_Package_Declaration
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Package_Declaration'Result then Self.Is_Declaration);

   not overriding function Is_Definition (Self : Element) return Boolean
     is abstract;

   not overriding function Is_Type_Definition (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Type_Definition'Result then Self.Is_Definition);

   not overriding function Is_Subtype_Indication
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Subtype_Indication'Result then Self.Is_Definition);

   not overriding function Is_Constraint (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Constraint'Result then Self.Is_Definition);

   not overriding function Is_Component_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Component_Definition'Result then Self.Is_Definition);

   not overriding function Is_Discrete_Range (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Discrete_Range'Result then Self.Is_Definition);

   not overriding function Is_Discrete_Subtype_Indication
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Discrete_Subtype_Indication'Result then Self.Is_Discrete_Range);

   not overriding function Is_Discrete_Range_Attribute_Reference
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Discrete_Range_Attribute_Reference'Result
          then Self.Is_Discrete_Range);

   not overriding function Is_Discrete_Simple_Expression_Range
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Discrete_Simple_Expression_Range'Result
          then Self.Is_Discrete_Range);

   not overriding function Is_Unknown_Discriminant_Part
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Unknown_Discriminant_Part'Result then Self.Is_Definition);

   not overriding function Is_Known_Discriminant_Part
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Known_Discriminant_Part'Result then Self.Is_Definition);

   not overriding function Is_Record_Definition (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_Record_Definition'Result then Self.Is_Definition);

   not overriding function Is_Null_Component (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Null_Component'Result then Self.Is_Definition);

   not overriding function Is_Variant_Part (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Variant_Part'Result then Self.Is_Definition);

   not overriding function Is_Variant (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Variant'Result then Self.Is_Definition);

   not overriding function Is_Others_Choice (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Others_Choice'Result then Self.Is_Definition);

   not overriding function Is_Anonymous_Access_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Anonymous_Access_Definition'Result then Self.Is_Definition);

   not overriding function Is_Anonymous_Access_To_Object
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Anonymous_Access_To_Object'Result
          then Self.Is_Anonymous_Access_Definition);

   not overriding function Is_Anonymous_Access_To_Procedure
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Anonymous_Access_To_Procedure'Result
          then Self.Is_Anonymous_Access_Definition);

   not overriding function Is_Anonymous_Access_To_Function
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Anonymous_Access_To_Function'Result
          then Self.Is_Anonymous_Access_Definition);

   not overriding function Is_Private_Type_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Private_Type_Definition'Result then Self.Is_Definition);

   not overriding function Is_Private_Extension_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Private_Extension_Definition'Result then Self.Is_Definition);

   not overriding function Is_Incomplete_Type_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Incomplete_Type_Definition'Result then Self.Is_Definition);

   not overriding function Is_Task_Definition (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Task_Definition'Result then Self.Is_Definition);

   not overriding function Is_Protected_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Protected_Definition'Result then Self.Is_Definition);

   not overriding function Is_Formal_Type_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Type_Definition'Result then Self.Is_Definition);

   not overriding function Is_Aspect_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Aspect_Specification'Result then Self.Is_Definition);

   not overriding function Is_Real_Range_Specification
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Real_Range_Specification'Result then Self.Is_Definition);

   not overriding function Is_Expression (Self : Element) return Boolean
     is abstract;

   not overriding function Is_Numeric_Literal (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Numeric_Literal'Result then Self.Is_Expression);

   not overriding function Is_String_Literal (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_String_Literal'Result then Self.Is_Expression);

   not overriding function Is_Identifier (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Identifier'Result then Self.Is_Expression);

   not overriding function Is_Operator_Symbol (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Operator_Symbol'Result then Self.Is_Expression);

   not overriding function Is_Character_Literal (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_Character_Literal'Result then Self.Is_Expression);

   not overriding function Is_Explicit_Dereference
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Explicit_Dereference'Result then Self.Is_Expression);

   not overriding function Is_Function_Call (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Function_Call'Result then Self.Is_Expression);

   not overriding function Is_Indexed_Component (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_Indexed_Component'Result then Self.Is_Expression);

   not overriding function Is_Slice (Self : Element) return Boolean is abstract
     with Post'Class => (if Is_Slice'Result then Self.Is_Expression);

   not overriding function Is_Selected_Component
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Selected_Component'Result then Self.Is_Expression);

   not overriding function Is_Attribute_Reference
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Attribute_Reference'Result then Self.Is_Expression);

   not overriding function Is_Record_Aggregate (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_Record_Aggregate'Result then Self.Is_Expression);

   not overriding function Is_Extension_Aggregate
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Extension_Aggregate'Result then Self.Is_Expression);

   not overriding function Is_Array_Aggregate (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Array_Aggregate'Result then Self.Is_Expression);

   not overriding function Is_Short_Circuit_Operation
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Short_Circuit_Operation'Result then Self.Is_Expression);

   not overriding function Is_Membership_Test (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Membership_Test'Result then Self.Is_Expression);

   not overriding function Is_Null_Literal (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Null_Literal'Result then Self.Is_Expression);

   not overriding function Is_Parenthesized_Expression
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Parenthesized_Expression'Result then Self.Is_Expression);

   not overriding function Is_Raise_Expression (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_Raise_Expression'Result then Self.Is_Expression);

   not overriding function Is_Type_Conversion (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Type_Conversion'Result then Self.Is_Expression);

   not overriding function Is_Qualified_Expression
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Qualified_Expression'Result then Self.Is_Expression);

   not overriding function Is_Allocator (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Allocator'Result then Self.Is_Expression);

   not overriding function Is_Case_Expression (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Case_Expression'Result then Self.Is_Expression);

   not overriding function Is_If_Expression (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_If_Expression'Result then Self.Is_Expression);

   not overriding function Is_Quantified_Expression
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Quantified_Expression'Result then Self.Is_Expression);

   not overriding function Is_Association (Self : Element) return Boolean
     is abstract;

   not overriding function Is_Discriminant_Association
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Discriminant_Association'Result then Self.Is_Association);

   not overriding function Is_Record_Component_Association
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Record_Component_Association'Result then Self.Is_Association);

   not overriding function Is_Array_Component_Association
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Array_Component_Association'Result then Self.Is_Association);

   not overriding function Is_Parameter_Association
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Parameter_Association'Result then Self.Is_Association);

   not overriding function Is_Formal_Package_Association
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Package_Association'Result then Self.Is_Association);

   not overriding function Is_Statement (Self : Element) return Boolean
     is abstract;

   not overriding function Is_Null_Statement (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Null_Statement'Result then Self.Is_Statement);

   not overriding function Is_Assignment_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Assignment_Statement'Result then Self.Is_Statement);

   not overriding function Is_If_Statement (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_If_Statement'Result then Self.Is_Statement);

   not overriding function Is_Case_Statement (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Case_Statement'Result then Self.Is_Statement);

   not overriding function Is_Loop_Statement (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Loop_Statement'Result then Self.Is_Statement);

   not overriding function Is_While_Loop_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_While_Loop_Statement'Result then Self.Is_Statement);

   not overriding function Is_For_Loop_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_For_Loop_Statement'Result then Self.Is_Statement);

   not overriding function Is_Block_Statement (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Block_Statement'Result then Self.Is_Statement);

   not overriding function Is_Exit_Statement (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Exit_Statement'Result then Self.Is_Statement);

   not overriding function Is_Goto_Statement (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Goto_Statement'Result then Self.Is_Statement);

   not overriding function Is_Call_Statement (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Call_Statement'Result then Self.Is_Statement);

   not overriding function Is_Simple_Return_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Simple_Return_Statement'Result then Self.Is_Statement);

   not overriding function Is_Extended_Return_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Extended_Return_Statement'Result then Self.Is_Statement);

   not overriding function Is_Accept_Statement (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Accept_Statement'Result then Self.Is_Statement);

   not overriding function Is_Requeue_Statement (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_Requeue_Statement'Result then Self.Is_Statement);

   not overriding function Is_Delay_Statement (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Delay_Statement'Result then Self.Is_Statement);

   not overriding function Is_Terminate_Alternative_Statement
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Terminate_Alternative_Statement'Result then Self.Is_Statement);

   not overriding function Is_Select_Statement (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Select_Statement'Result then Self.Is_Statement);

   not overriding function Is_Abort_Statement (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Abort_Statement'Result then Self.Is_Statement);

   not overriding function Is_Raise_Statement (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Raise_Statement'Result then Self.Is_Statement);

   not overriding function Is_Code_Statement (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Code_Statement'Result then Self.Is_Statement);

   not overriding function Is_Path (Self : Element) return Boolean is abstract;

   not overriding function Is_Elsif_Path (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Elsif_Path'Result then Self.Is_Path);

   not overriding function Is_Case_Path (Self : Element) return Boolean
     is abstract with Post'Class => (if Is_Case_Path'Result then Self.Is_Path);

   not overriding function Is_Select_Path (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Select_Path'Result then Self.Is_Path);

   not overriding function Is_Case_Expression_Path
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Case_Expression_Path'Result then Self.Is_Path);

   not overriding function Is_Elsif_Expression_Path
    (Self : Element)
      return Boolean is abstract
     with Post'Class => (if Is_Elsif_Expression_Path'Result then Self.Is_Path);

   not overriding function Is_Clause (Self : Element) return Boolean
     is abstract;

   not overriding function Is_Use_Clause (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Use_Clause'Result then Self.Is_Clause);

   not overriding function Is_With_Clause (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_With_Clause'Result then Self.Is_Clause);

   not overriding function Is_Representation_Clause
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Representation_Clause'Result then Self.Is_Clause);

   not overriding function Is_Component_Clause (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Component_Clause'Result then Self.Is_Clause);

   not overriding function Is_Derived_Type (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_Derived_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Derived_Record_Extension
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Derived_Record_Extension'Result then Self.Is_Type_Definition);

   not overriding function Is_Enumeration_Type (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_Enumeration_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Signed_Integer_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Signed_Integer_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Modular_Type (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_Modular_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Root_Type (Self : Element) return Boolean
     is abstract
     with Post'Class => (if Is_Root_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Floating_Point_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Floating_Point_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Ordinary_Fixed_Point_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Ordinary_Fixed_Point_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Decimal_Fixed_Point_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Decimal_Fixed_Point_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Unconstrained_Array_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Unconstrained_Array_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Constrained_Array_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Constrained_Array_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Record_Type (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_Record_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Interface_Type (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_Interface_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Object_Access_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Object_Access_Type'Result then Self.Is_Access_Type);

   not overriding function Is_Procedure_Access_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Procedure_Access_Type'Result then Self.Is_Access_Type);

   not overriding function Is_Function_Access_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Function_Access_Type'Result then Self.Is_Access_Type);

   not overriding function Is_Formal_Private_Type_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Private_Type_Definition'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Derived_Type_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Derived_Type_Definition'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Discrete_Type_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Discrete_Type_Definition'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Signed_Integer_Type_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Signed_Integer_Type_Definition'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Modular_Type_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Modular_Type_Definition'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Floating_Point_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Floating_Point_Definition'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Ordinary_Fixed_Point_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Ordinary_Fixed_Point_Definition'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Decimal_Fixed_Point_Definition
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Decimal_Fixed_Point_Definition'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Unconstrained_Array_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Unconstrained_Array_Type'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Constrained_Array_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Constrained_Array_Type'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Access_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Access_Type'Result then Self.Is_Formal_Type_Definition);

   not overriding function Is_Formal_Object_Access_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Object_Access_Type'Result
          then Self.Is_Formal_Access_Type);

   not overriding function Is_Formal_Procedure_Access_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Procedure_Access_Type'Result
          then Self.Is_Formal_Access_Type);

   not overriding function Is_Formal_Function_Access_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Function_Access_Type'Result
          then Self.Is_Formal_Access_Type);

   not overriding function Is_Formal_Interface_Type
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Formal_Interface_Type'Result
          then Self.Is_Formal_Type_Definition);

   not overriding function Is_Access_Type (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_Access_Type'Result then Self.Is_Type_Definition);

   not overriding function Is_Range_Attribute_Reference
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Range_Attribute_Reference'Result then Self.Is_Constraint);

   not overriding function Is_Simple_Expression_Range
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Simple_Expression_Range'Result then Self.Is_Constraint);

   not overriding function Is_Digits_Constraint (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_Digits_Constraint'Result then Self.Is_Constraint);

   not overriding function Is_Delta_Constraint (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_Delta_Constraint'Result then Self.Is_Constraint);

   not overriding function Is_Index_Constraint (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_Index_Constraint'Result then Self.Is_Constraint);

   not overriding function Is_Discriminant_Constraint
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Discriminant_Constraint'Result then Self.Is_Constraint);

   not overriding function Is_Attribute_Definition_Clause
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Attribute_Definition_Clause'Result
          then Self.Is_Representation_Clause);

   not overriding function Is_Enumeration_Representation_Clause
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Enumeration_Representation_Clause'Result
          then Self.Is_Representation_Clause);

   not overriding function Is_Record_Representation_Clause
    (Self : Element)
      return Boolean is abstract
     with Post'Class =>
       (if Is_Record_Representation_Clause'Result
          then Self.Is_Representation_Clause);

   not overriding function Is_At_Clause (Self : Element) return Boolean
     is abstract
     with Post'Class =>
       (if Is_At_Clause'Result then Self.Is_Representation_Clause);

   not overriding function Is_Exception_Handler (Self : Element) return Boolean
     is abstract;

   function To_Pragma
    (Self : access Element'Class)
      return Program.Elements.Pragmas.Pragma_Access with Pre => Self.Is_Pragma;

   function To_Defining_Name
    (Self : access Element'Class)
      return Program.Elements.Defining_Names.Defining_Name_Access
     with Pre => Self.Is_Defining_Name;

   function To_Defining_Identifier
    (Self : access Element'Class)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
     with Pre => Self.Is_Defining_Identifier;

   function To_Defining_Character_Literal
    (Self : access Element'Class)
      return Program.Elements.Defining_Character_Literals
          .Defining_Character_Literal_Access
     with Pre => Self.Is_Defining_Character_Literal;

   function To_Defining_Operator_Symbol
    (Self : access Element'Class)
      return Program.Elements.Defining_Operator_Symbols
          .Defining_Operator_Symbol_Access
     with Pre => Self.Is_Defining_Operator_Symbol;

   function To_Defining_Expanded_Name
    (Self : access Element'Class)
      return Program.Elements.Defining_Expanded_Names
          .Defining_Expanded_Name_Access
     with Pre => Self.Is_Defining_Expanded_Name;

   function To_Declaration
    (Self : access Element'Class)
      return Program.Elements.Declarations.Declaration_Access
     with Pre => Self.Is_Declaration;

   function To_Type_Declaration
    (Self : access Element'Class)
      return Program.Elements.Type_Declarations.Type_Declaration_Access
     with Pre => Self.Is_Type_Declaration;

   function To_Task_Type_Declaration
    (Self : access Element'Class)
      return Program.Elements.Task_Type_Declarations
          .Task_Type_Declaration_Access
     with Pre => Self.Is_Task_Type_Declaration;

   function To_Protected_Type_Declaration
    (Self : access Element'Class)
      return Program.Elements.Protected_Type_Declarations
          .Protected_Type_Declaration_Access
     with Pre => Self.Is_Protected_Type_Declaration;

   function To_Subtype_Declaration
    (Self : access Element'Class)
      return Program.Elements.Subtype_Declarations.Subtype_Declaration_Access
     with Pre => Self.Is_Subtype_Declaration;

   function To_Object_Declaration
    (Self : access Element'Class)
      return Program.Elements.Object_Declarations.Object_Declaration_Access
     with Pre => Self.Is_Object_Declaration;

   function To_Single_Task_Declaration
    (Self : access Element'Class)
      return Program.Elements.Single_Task_Declarations
          .Single_Task_Declaration_Access
     with Pre => Self.Is_Single_Task_Declaration;

   function To_Single_Protected_Declaration
    (Self : access Element'Class)
      return Program.Elements.Single_Protected_Declarations
          .Single_Protected_Declaration_Access
     with Pre => Self.Is_Single_Protected_Declaration;

   function To_Number_Declaration
    (Self : access Element'Class)
      return Program.Elements.Number_Declarations.Number_Declaration_Access
     with Pre => Self.Is_Number_Declaration;

   function To_Enumeration_Literal_Specification
    (Self : access Element'Class)
      return Program.Elements.Enumeration_Literal_Specifications
          .Enumeration_Literal_Specification_Access
     with Pre => Self.Is_Enumeration_Literal_Specification;

   function To_Discriminant_Specification
    (Self : access Element'Class)
      return Program.Elements.Discriminant_Specifications
          .Discriminant_Specification_Access
     with Pre => Self.Is_Discriminant_Specification;

   function To_Component_Declaration
    (Self : access Element'Class)
      return Program.Elements.Component_Declarations
          .Component_Declaration_Access
     with Pre => Self.Is_Component_Declaration;

   function To_Loop_Parameter_Specification
    (Self : access Element'Class)
      return Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification_Access
     with Pre => Self.Is_Loop_Parameter_Specification;

   function To_Generalized_Iterator_Specification
    (Self : access Element'Class)
      return Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification_Access
     with Pre => Self.Is_Generalized_Iterator_Specification;

   function To_Element_Iterator_Specification
    (Self : access Element'Class)
      return Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification_Access
     with Pre => Self.Is_Element_Iterator_Specification;

   function To_Procedure_Declaration
    (Self : access Element'Class)
      return Program.Elements.Procedure_Declarations
          .Procedure_Declaration_Access
     with Pre => Self.Is_Procedure_Declaration;

   function To_Function_Declaration
    (Self : access Element'Class)
      return Program.Elements.Function_Declarations.Function_Declaration_Access
     with Pre => Self.Is_Function_Declaration;

   function To_Parameter_Specification
    (Self : access Element'Class)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Access
     with Pre => Self.Is_Parameter_Specification;

   function To_Procedure_Body_Declaration
    (Self : access Element'Class)
      return Program.Elements.Procedure_Body_Declarations
          .Procedure_Body_Declaration_Access
     with Pre => Self.Is_Procedure_Body_Declaration;

   function To_Function_Body_Declaration
    (Self : access Element'Class)
      return Program.Elements.Function_Body_Declarations
          .Function_Body_Declaration_Access
     with Pre => Self.Is_Function_Body_Declaration;

   function To_Return_Object_Specification
    (Self : access Element'Class)
      return Program.Elements.Return_Object_Specifications
          .Return_Object_Specification_Access
     with Pre => Self.Is_Return_Object_Specification;

   function To_Package_Declaration
    (Self : access Element'Class)
      return Program.Elements.Package_Declarations.Package_Declaration_Access
     with Pre => Self.Is_Package_Declaration;

   function To_Package_Body_Declaration
    (Self : access Element'Class)
      return Program.Elements.Package_Body_Declarations
          .Package_Body_Declaration_Access
     with Pre => Self.Is_Package_Body_Declaration;

   function To_Object_Renaming_Declaration
    (Self : access Element'Class)
      return Program.Elements.Object_Renaming_Declarations
          .Object_Renaming_Declaration_Access
     with Pre => Self.Is_Object_Renaming_Declaration;

   function To_Exception_Renaming_Declaration
    (Self : access Element'Class)
      return Program.Elements.Exception_Renaming_Declarations
          .Exception_Renaming_Declaration_Access
     with Pre => Self.Is_Exception_Renaming_Declaration;

   function To_Procedure_Renaming_Declaration
    (Self : access Element'Class)
      return Program.Elements.Procedure_Renaming_Declarations
          .Procedure_Renaming_Declaration_Access
     with Pre => Self.Is_Procedure_Renaming_Declaration;

   function To_Function_Renaming_Declaration
    (Self : access Element'Class)
      return Program.Elements.Function_Renaming_Declarations
          .Function_Renaming_Declaration_Access
     with Pre => Self.Is_Function_Renaming_Declaration;

   function To_Package_Renaming_Declaration
    (Self : access Element'Class)
      return Program.Elements.Package_Renaming_Declarations
          .Package_Renaming_Declaration_Access
     with Pre => Self.Is_Package_Renaming_Declaration;

   function To_Generic_Package_Renaming_Declaration
    (Self : access Element'Class)
      return Program.Elements.Generic_Package_Renaming_Declarations
          .Generic_Package_Renaming_Declaration_Access
     with Pre => Self.Is_Generic_Package_Renaming_Declaration;

   function To_Generic_Procedure_Renaming_Declaration
    (Self : access Element'Class)
      return Program.Elements.Generic_Procedure_Renaming_Declarations
          .Generic_Procedure_Renaming_Declaration_Access
     with Pre => Self.Is_Generic_Procedure_Renaming_Declaration;

   function To_Generic_Function_Renaming_Declaration
    (Self : access Element'Class)
      return Program.Elements.Generic_Function_Renaming_Declarations
          .Generic_Function_Renaming_Declaration_Access
     with Pre => Self.Is_Generic_Function_Renaming_Declaration;

   function To_Task_Body_Declaration
    (Self : access Element'Class)
      return Program.Elements.Task_Body_Declarations
          .Task_Body_Declaration_Access
     with Pre => Self.Is_Task_Body_Declaration;

   function To_Protected_Body_Declaration
    (Self : access Element'Class)
      return Program.Elements.Protected_Body_Declarations
          .Protected_Body_Declaration_Access
     with Pre => Self.Is_Protected_Body_Declaration;

   function To_Entry_Declaration
    (Self : access Element'Class)
      return Program.Elements.Entry_Declarations.Entry_Declaration_Access
     with Pre => Self.Is_Entry_Declaration;

   function To_Entry_Body_Declaration
    (Self : access Element'Class)
      return Program.Elements.Entry_Body_Declarations
          .Entry_Body_Declaration_Access
     with Pre => Self.Is_Entry_Body_Declaration;

   function To_Entry_Index_Specification
    (Self : access Element'Class)
      return Program.Elements.Entry_Index_Specifications
          .Entry_Index_Specification_Access
     with Pre => Self.Is_Entry_Index_Specification;

   function To_Procedure_Body_Stub
    (Self : access Element'Class)
      return Program.Elements.Procedure_Body_Stubs.Procedure_Body_Stub_Access
     with Pre => Self.Is_Procedure_Body_Stub;

   function To_Function_Body_Stub
    (Self : access Element'Class)
      return Program.Elements.Function_Body_Stubs.Function_Body_Stub_Access
     with Pre => Self.Is_Function_Body_Stub;

   function To_Package_Body_Stub
    (Self : access Element'Class)
      return Program.Elements.Package_Body_Stubs.Package_Body_Stub_Access
     with Pre => Self.Is_Package_Body_Stub;

   function To_Task_Body_Stub
    (Self : access Element'Class)
      return Program.Elements.Task_Body_Stubs.Task_Body_Stub_Access
     with Pre => Self.Is_Task_Body_Stub;

   function To_Protected_Body_Stub
    (Self : access Element'Class)
      return Program.Elements.Protected_Body_Stubs.Protected_Body_Stub_Access
     with Pre => Self.Is_Protected_Body_Stub;

   function To_Exception_Declaration
    (Self : access Element'Class)
      return Program.Elements.Exception_Declarations
          .Exception_Declaration_Access
     with Pre => Self.Is_Exception_Declaration;

   function To_Choice_Parameter_Specification
    (Self : access Element'Class)
      return Program.Elements.Choice_Parameter_Specifications
          .Choice_Parameter_Specification_Access
     with Pre => Self.Is_Choice_Parameter_Specification;

   function To_Generic_Package_Declaration
    (Self : access Element'Class)
      return Program.Elements.Generic_Package_Declarations
          .Generic_Package_Declaration_Access
     with Pre => Self.Is_Generic_Package_Declaration;

   function To_Generic_Procedure_Declaration
    (Self : access Element'Class)
      return Program.Elements.Generic_Procedure_Declarations
          .Generic_Procedure_Declaration_Access
     with Pre => Self.Is_Generic_Procedure_Declaration;

   function To_Generic_Function_Declaration
    (Self : access Element'Class)
      return Program.Elements.Generic_Function_Declarations
          .Generic_Function_Declaration_Access
     with Pre => Self.Is_Generic_Function_Declaration;

   function To_Package_Instantiation
    (Self : access Element'Class)
      return Program.Elements.Package_Instantiations
          .Package_Instantiation_Access
     with Pre => Self.Is_Package_Instantiation;

   function To_Procedure_Instantiation
    (Self : access Element'Class)
      return Program.Elements.Procedure_Instantiations
          .Procedure_Instantiation_Access
     with Pre => Self.Is_Procedure_Instantiation;

   function To_Function_Instantiation
    (Self : access Element'Class)
      return Program.Elements.Function_Instantiations
          .Function_Instantiation_Access
     with Pre => Self.Is_Function_Instantiation;

   function To_Formal_Object_Declaration
    (Self : access Element'Class)
      return Program.Elements.Formal_Object_Declarations
          .Formal_Object_Declaration_Access
     with Pre => Self.Is_Formal_Object_Declaration;

   function To_Formal_Type_Declaration
    (Self : access Element'Class)
      return Program.Elements.Formal_Type_Declarations
          .Formal_Type_Declaration_Access
     with Pre => Self.Is_Formal_Type_Declaration;

   function To_Formal_Procedure_Declaration
    (Self : access Element'Class)
      return Program.Elements.Formal_Procedure_Declarations
          .Formal_Procedure_Declaration_Access
     with Pre => Self.Is_Formal_Procedure_Declaration;

   function To_Formal_Function_Declaration
    (Self : access Element'Class)
      return Program.Elements.Formal_Function_Declarations
          .Formal_Function_Declaration_Access
     with Pre => Self.Is_Formal_Function_Declaration;

   function To_Formal_Package_Declaration
    (Self : access Element'Class)
      return Program.Elements.Formal_Package_Declarations
          .Formal_Package_Declaration_Access
     with Pre => Self.Is_Formal_Package_Declaration;

   function To_Definition
    (Self : access Element'Class)
      return Program.Elements.Definitions.Definition_Access
     with Pre => Self.Is_Definition;

   function To_Type_Definition
    (Self : access Element'Class)
      return Program.Elements.Type_Definitions.Type_Definition_Access
     with Pre => Self.Is_Type_Definition;

   function To_Subtype_Indication
    (Self : access Element'Class)
      return Program.Elements.Subtype_Indications.Subtype_Indication_Access
     with Pre => Self.Is_Subtype_Indication;

   function To_Constraint
    (Self : access Element'Class)
      return Program.Elements.Constraints.Constraint_Access
     with Pre => Self.Is_Constraint;

   function To_Component_Definition
    (Self : access Element'Class)
      return Program.Elements.Component_Definitions.Component_Definition_Access
     with Pre => Self.Is_Component_Definition;

   function To_Discrete_Range
    (Self : access Element'Class)
      return Program.Elements.Discrete_Ranges.Discrete_Range_Access
     with Pre => Self.Is_Discrete_Range;

   function To_Discrete_Subtype_Indication
    (Self : access Element'Class)
      return Program.Elements.Discrete_Subtype_Indications
          .Discrete_Subtype_Indication_Access
     with Pre => Self.Is_Discrete_Subtype_Indication;

   function To_Discrete_Range_Attribute_Reference
    (Self : access Element'Class)
      return Program.Elements.Discrete_Range_Attribute_References
          .Discrete_Range_Attribute_Reference_Access
     with Pre => Self.Is_Discrete_Range_Attribute_Reference;

   function To_Discrete_Simple_Expression_Range
    (Self : access Element'Class)
      return Program.Elements.Discrete_Simple_Expression_Ranges
          .Discrete_Simple_Expression_Range_Access
     with Pre => Self.Is_Discrete_Simple_Expression_Range;

   function To_Unknown_Discriminant_Part
    (Self : access Element'Class)
      return Program.Elements.Unknown_Discriminant_Parts
          .Unknown_Discriminant_Part_Access
     with Pre => Self.Is_Unknown_Discriminant_Part;

   function To_Known_Discriminant_Part
    (Self : access Element'Class)
      return Program.Elements.Known_Discriminant_Parts
          .Known_Discriminant_Part_Access
     with Pre => Self.Is_Known_Discriminant_Part;

   function To_Record_Definition
    (Self : access Element'Class)
      return Program.Elements.Record_Definitions.Record_Definition_Access
     with Pre => Self.Is_Record_Definition;

   function To_Null_Component
    (Self : access Element'Class)
      return Program.Elements.Null_Components.Null_Component_Access
     with Pre => Self.Is_Null_Component;

   function To_Variant_Part
    (Self : access Element'Class)
      return Program.Elements.Variant_Parts.Variant_Part_Access
     with Pre => Self.Is_Variant_Part;

   function To_Variant
    (Self : access Element'Class)
      return Program.Elements.Variants.Variant_Access
     with Pre => Self.Is_Variant;

   function To_Others_Choice
    (Self : access Element'Class)
      return Program.Elements.Others_Choices.Others_Choice_Access
     with Pre => Self.Is_Others_Choice;

   function To_Anonymous_Access_Definition
    (Self : access Element'Class)
      return Program.Elements.Anonymous_Access_Definitions
          .Anonymous_Access_Definition_Access
     with Pre => Self.Is_Anonymous_Access_Definition;

   function To_Anonymous_Access_To_Object
    (Self : access Element'Class)
      return Program.Elements.Anonymous_Access_To_Objects
          .Anonymous_Access_To_Object_Access
     with Pre => Self.Is_Anonymous_Access_To_Object;

   function To_Anonymous_Access_To_Procedure
    (Self : access Element'Class)
      return Program.Elements.Anonymous_Access_To_Procedures
          .Anonymous_Access_To_Procedure_Access
     with Pre => Self.Is_Anonymous_Access_To_Procedure;

   function To_Anonymous_Access_To_Function
    (Self : access Element'Class)
      return Program.Elements.Anonymous_Access_To_Functions
          .Anonymous_Access_To_Function_Access
     with Pre => Self.Is_Anonymous_Access_To_Function;

   function To_Private_Type_Definition
    (Self : access Element'Class)
      return Program.Elements.Private_Type_Definitions
          .Private_Type_Definition_Access
     with Pre => Self.Is_Private_Type_Definition;

   function To_Private_Extension_Definition
    (Self : access Element'Class)
      return Program.Elements.Private_Extension_Definitions
          .Private_Extension_Definition_Access
     with Pre => Self.Is_Private_Extension_Definition;

   function To_Incomplete_Type_Definition
    (Self : access Element'Class)
      return Program.Elements.Incomplete_Type_Definitions
          .Incomplete_Type_Definition_Access
     with Pre => Self.Is_Incomplete_Type_Definition;

   function To_Task_Definition
    (Self : access Element'Class)
      return Program.Elements.Task_Definitions.Task_Definition_Access
     with Pre => Self.Is_Task_Definition;

   function To_Protected_Definition
    (Self : access Element'Class)
      return Program.Elements.Protected_Definitions.Protected_Definition_Access
     with Pre => Self.Is_Protected_Definition;

   function To_Formal_Type_Definition
    (Self : access Element'Class)
      return Program.Elements.Formal_Type_Definitions
          .Formal_Type_Definition_Access
     with Pre => Self.Is_Formal_Type_Definition;

   function To_Aspect_Specification
    (Self : access Element'Class)
      return Program.Elements.Aspect_Specifications.Aspect_Specification_Access
     with Pre => Self.Is_Aspect_Specification;

   function To_Real_Range_Specification
    (Self : access Element'Class)
      return Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Access
     with Pre => Self.Is_Real_Range_Specification;

   function To_Expression
    (Self : access Element'Class)
      return Program.Elements.Expressions.Expression_Access
     with Pre => Self.Is_Expression;

   function To_Numeric_Literal
    (Self : access Element'Class)
      return Program.Elements.Numeric_Literals.Numeric_Literal_Access
     with Pre => Self.Is_Numeric_Literal;

   function To_String_Literal
    (Self : access Element'Class)
      return Program.Elements.String_Literals.String_Literal_Access
     with Pre => Self.Is_String_Literal;

   function To_Identifier
    (Self : access Element'Class)
      return Program.Elements.Identifiers.Identifier_Access
     with Pre => Self.Is_Identifier;

   function To_Operator_Symbol
    (Self : access Element'Class)
      return Program.Elements.Operator_Symbols.Operator_Symbol_Access
     with Pre => Self.Is_Operator_Symbol;

   function To_Character_Literal
    (Self : access Element'Class)
      return Program.Elements.Character_Literals.Character_Literal_Access
     with Pre => Self.Is_Character_Literal;

   function To_Explicit_Dereference
    (Self : access Element'Class)
      return Program.Elements.Explicit_Dereferences.Explicit_Dereference_Access
     with Pre => Self.Is_Explicit_Dereference;

   function To_Function_Call
    (Self : access Element'Class)
      return Program.Elements.Function_Calls.Function_Call_Access
     with Pre => Self.Is_Function_Call;

   function To_Indexed_Component
    (Self : access Element'Class)
      return Program.Elements.Indexed_Components.Indexed_Component_Access
     with Pre => Self.Is_Indexed_Component;

   function To_Slice
    (Self : access Element'Class)
      return Program.Elements.Slices.Slice_Access with Pre => Self.Is_Slice;

   function To_Selected_Component
    (Self : access Element'Class)
      return Program.Elements.Selected_Components.Selected_Component_Access
     with Pre => Self.Is_Selected_Component;

   function To_Attribute_Reference
    (Self : access Element'Class)
      return Program.Elements.Attribute_References.Attribute_Reference_Access
     with Pre => Self.Is_Attribute_Reference;

   function To_Record_Aggregate
    (Self : access Element'Class)
      return Program.Elements.Record_Aggregates.Record_Aggregate_Access
     with Pre => Self.Is_Record_Aggregate;

   function To_Extension_Aggregate
    (Self : access Element'Class)
      return Program.Elements.Extension_Aggregates.Extension_Aggregate_Access
     with Pre => Self.Is_Extension_Aggregate;

   function To_Array_Aggregate
    (Self : access Element'Class)
      return Program.Elements.Array_Aggregates.Array_Aggregate_Access
     with Pre => Self.Is_Array_Aggregate;

   function To_Short_Circuit_Operation
    (Self : access Element'Class)
      return Program.Elements.Short_Circuit_Operations
          .Short_Circuit_Operation_Access
     with Pre => Self.Is_Short_Circuit_Operation;

   function To_Membership_Test
    (Self : access Element'Class)
      return Program.Elements.Membership_Tests.Membership_Test_Access
     with Pre => Self.Is_Membership_Test;

   function To_Null_Literal
    (Self : access Element'Class)
      return Program.Elements.Null_Literals.Null_Literal_Access
     with Pre => Self.Is_Null_Literal;

   function To_Parenthesized_Expression
    (Self : access Element'Class)
      return Program.Elements.Parenthesized_Expressions
          .Parenthesized_Expression_Access
     with Pre => Self.Is_Parenthesized_Expression;

   function To_Raise_Expression
    (Self : access Element'Class)
      return Program.Elements.Raise_Expressions.Raise_Expression_Access
     with Pre => Self.Is_Raise_Expression;

   function To_Type_Conversion
    (Self : access Element'Class)
      return Program.Elements.Type_Conversions.Type_Conversion_Access
     with Pre => Self.Is_Type_Conversion;

   function To_Qualified_Expression
    (Self : access Element'Class)
      return Program.Elements.Qualified_Expressions.Qualified_Expression_Access
     with Pre => Self.Is_Qualified_Expression;

   function To_Allocator
    (Self : access Element'Class)
      return Program.Elements.Allocators.Allocator_Access
     with Pre => Self.Is_Allocator;

   function To_Case_Expression
    (Self : access Element'Class)
      return Program.Elements.Case_Expressions.Case_Expression_Access
     with Pre => Self.Is_Case_Expression;

   function To_If_Expression
    (Self : access Element'Class)
      return Program.Elements.If_Expressions.If_Expression_Access
     with Pre => Self.Is_If_Expression;

   function To_Quantified_Expression
    (Self : access Element'Class)
      return Program.Elements.Quantified_Expressions
          .Quantified_Expression_Access
     with Pre => Self.Is_Quantified_Expression;

   function To_Association
    (Self : access Element'Class)
      return Program.Elements.Associations.Association_Access
     with Pre => Self.Is_Association;

   function To_Discriminant_Association
    (Self : access Element'Class)
      return Program.Elements.Discriminant_Associations
          .Discriminant_Association_Access
     with Pre => Self.Is_Discriminant_Association;

   function To_Record_Component_Association
    (Self : access Element'Class)
      return Program.Elements.Record_Component_Associations
          .Record_Component_Association_Access
     with Pre => Self.Is_Record_Component_Association;

   function To_Array_Component_Association
    (Self : access Element'Class)
      return Program.Elements.Array_Component_Associations
          .Array_Component_Association_Access
     with Pre => Self.Is_Array_Component_Association;

   function To_Parameter_Association
    (Self : access Element'Class)
      return Program.Elements.Parameter_Associations
          .Parameter_Association_Access
     with Pre => Self.Is_Parameter_Association;

   function To_Formal_Package_Association
    (Self : access Element'Class)
      return Program.Elements.Formal_Package_Associations
          .Formal_Package_Association_Access
     with Pre => Self.Is_Formal_Package_Association;

   function To_Statement
    (Self : access Element'Class)
      return Program.Elements.Statements.Statement_Access
     with Pre => Self.Is_Statement;

   function To_Null_Statement
    (Self : access Element'Class)
      return Program.Elements.Null_Statements.Null_Statement_Access
     with Pre => Self.Is_Null_Statement;

   function To_Assignment_Statement
    (Self : access Element'Class)
      return Program.Elements.Assignment_Statements.Assignment_Statement_Access
     with Pre => Self.Is_Assignment_Statement;

   function To_If_Statement
    (Self : access Element'Class)
      return Program.Elements.If_Statements.If_Statement_Access
     with Pre => Self.Is_If_Statement;

   function To_Case_Statement
    (Self : access Element'Class)
      return Program.Elements.Case_Statements.Case_Statement_Access
     with Pre => Self.Is_Case_Statement;

   function To_Loop_Statement
    (Self : access Element'Class)
      return Program.Elements.Loop_Statements.Loop_Statement_Access
     with Pre => Self.Is_Loop_Statement;

   function To_While_Loop_Statement
    (Self : access Element'Class)
      return Program.Elements.While_Loop_Statements.While_Loop_Statement_Access
     with Pre => Self.Is_While_Loop_Statement;

   function To_For_Loop_Statement
    (Self : access Element'Class)
      return Program.Elements.For_Loop_Statements.For_Loop_Statement_Access
     with Pre => Self.Is_For_Loop_Statement;

   function To_Block_Statement
    (Self : access Element'Class)
      return Program.Elements.Block_Statements.Block_Statement_Access
     with Pre => Self.Is_Block_Statement;

   function To_Exit_Statement
    (Self : access Element'Class)
      return Program.Elements.Exit_Statements.Exit_Statement_Access
     with Pre => Self.Is_Exit_Statement;

   function To_Goto_Statement
    (Self : access Element'Class)
      return Program.Elements.Goto_Statements.Goto_Statement_Access
     with Pre => Self.Is_Goto_Statement;

   function To_Call_Statement
    (Self : access Element'Class)
      return Program.Elements.Call_Statements.Call_Statement_Access
     with Pre => Self.Is_Call_Statement;

   function To_Simple_Return_Statement
    (Self : access Element'Class)
      return Program.Elements.Simple_Return_Statements
          .Simple_Return_Statement_Access
     with Pre => Self.Is_Simple_Return_Statement;

   function To_Extended_Return_Statement
    (Self : access Element'Class)
      return Program.Elements.Extended_Return_Statements
          .Extended_Return_Statement_Access
     with Pre => Self.Is_Extended_Return_Statement;

   function To_Accept_Statement
    (Self : access Element'Class)
      return Program.Elements.Accept_Statements.Accept_Statement_Access
     with Pre => Self.Is_Accept_Statement;

   function To_Requeue_Statement
    (Self : access Element'Class)
      return Program.Elements.Requeue_Statements.Requeue_Statement_Access
     with Pre => Self.Is_Requeue_Statement;

   function To_Delay_Statement
    (Self : access Element'Class)
      return Program.Elements.Delay_Statements.Delay_Statement_Access
     with Pre => Self.Is_Delay_Statement;

   function To_Terminate_Alternative_Statement
    (Self : access Element'Class)
      return Program.Elements.Terminate_Alternative_Statements
          .Terminate_Alternative_Statement_Access
     with Pre => Self.Is_Terminate_Alternative_Statement;

   function To_Select_Statement
    (Self : access Element'Class)
      return Program.Elements.Select_Statements.Select_Statement_Access
     with Pre => Self.Is_Select_Statement;

   function To_Abort_Statement
    (Self : access Element'Class)
      return Program.Elements.Abort_Statements.Abort_Statement_Access
     with Pre => Self.Is_Abort_Statement;

   function To_Raise_Statement
    (Self : access Element'Class)
      return Program.Elements.Raise_Statements.Raise_Statement_Access
     with Pre => Self.Is_Raise_Statement;

   function To_Code_Statement
    (Self : access Element'Class)
      return Program.Elements.Code_Statements.Code_Statement_Access
     with Pre => Self.Is_Code_Statement;

   function To_Path
    (Self : access Element'Class)
      return Program.Elements.Paths.Path_Access with Pre => Self.Is_Path;

   function To_Elsif_Path
    (Self : access Element'Class)
      return Program.Elements.Elsif_Paths.Elsif_Path_Access
     with Pre => Self.Is_Elsif_Path;

   function To_Case_Path
    (Self : access Element'Class)
      return Program.Elements.Case_Paths.Case_Path_Access
     with Pre => Self.Is_Case_Path;

   function To_Select_Path
    (Self : access Element'Class)
      return Program.Elements.Select_Paths.Select_Path_Access
     with Pre => Self.Is_Select_Path;

   function To_Case_Expression_Path
    (Self : access Element'Class)
      return Program.Elements.Case_Expression_Paths.Case_Expression_Path_Access
     with Pre => Self.Is_Case_Expression_Path;

   function To_Elsif_Expression_Path
    (Self : access Element'Class)
      return Program.Elements.Elsif_Expression_Paths
          .Elsif_Expression_Path_Access
     with Pre => Self.Is_Elsif_Expression_Path;

   function To_Clause
    (Self : access Element'Class)
      return Program.Elements.Clauses.Clause_Access with Pre => Self.Is_Clause;

   function To_Use_Clause
    (Self : access Element'Class)
      return Program.Elements.Use_Clauses.Use_Clause_Access
     with Pre => Self.Is_Use_Clause;

   function To_With_Clause
    (Self : access Element'Class)
      return Program.Elements.With_Clauses.With_Clause_Access
     with Pre => Self.Is_With_Clause;

   function To_Representation_Clause
    (Self : access Element'Class)
      return Program.Elements.Representation_Clauses
          .Representation_Clause_Access
     with Pre => Self.Is_Representation_Clause;

   function To_Component_Clause
    (Self : access Element'Class)
      return Program.Elements.Component_Clauses.Component_Clause_Access
     with Pre => Self.Is_Component_Clause;

   function To_Derived_Type
    (Self : access Element'Class)
      return Program.Elements.Derived_Types.Derived_Type_Access
     with Pre => Self.Is_Derived_Type;

   function To_Derived_Record_Extension
    (Self : access Element'Class)
      return Program.Elements.Derived_Record_Extensions
          .Derived_Record_Extension_Access
     with Pre => Self.Is_Derived_Record_Extension;

   function To_Enumeration_Type
    (Self : access Element'Class)
      return Program.Elements.Enumeration_Types.Enumeration_Type_Access
     with Pre => Self.Is_Enumeration_Type;

   function To_Signed_Integer_Type
    (Self : access Element'Class)
      return Program.Elements.Signed_Integer_Types.Signed_Integer_Type_Access
     with Pre => Self.Is_Signed_Integer_Type;

   function To_Modular_Type
    (Self : access Element'Class)
      return Program.Elements.Modular_Types.Modular_Type_Access
     with Pre => Self.Is_Modular_Type;

   function To_Root_Type
    (Self : access Element'Class)
      return Program.Elements.Root_Types.Root_Type_Access
     with Pre => Self.Is_Root_Type;

   function To_Floating_Point_Type
    (Self : access Element'Class)
      return Program.Elements.Floating_Point_Types.Floating_Point_Type_Access
     with Pre => Self.Is_Floating_Point_Type;

   function To_Ordinary_Fixed_Point_Type
    (Self : access Element'Class)
      return Program.Elements.Ordinary_Fixed_Point_Types
          .Ordinary_Fixed_Point_Type_Access
     with Pre => Self.Is_Ordinary_Fixed_Point_Type;

   function To_Decimal_Fixed_Point_Type
    (Self : access Element'Class)
      return Program.Elements.Decimal_Fixed_Point_Types
          .Decimal_Fixed_Point_Type_Access
     with Pre => Self.Is_Decimal_Fixed_Point_Type;

   function To_Unconstrained_Array_Type
    (Self : access Element'Class)
      return Program.Elements.Unconstrained_Array_Types
          .Unconstrained_Array_Type_Access
     with Pre => Self.Is_Unconstrained_Array_Type;

   function To_Constrained_Array_Type
    (Self : access Element'Class)
      return Program.Elements.Constrained_Array_Types
          .Constrained_Array_Type_Access
     with Pre => Self.Is_Constrained_Array_Type;

   function To_Record_Type
    (Self : access Element'Class)
      return Program.Elements.Record_Types.Record_Type_Access
     with Pre => Self.Is_Record_Type;

   function To_Interface_Type
    (Self : access Element'Class)
      return Program.Elements.Interface_Types.Interface_Type_Access
     with Pre => Self.Is_Interface_Type;

   function To_Object_Access_Type
    (Self : access Element'Class)
      return Program.Elements.Object_Access_Types.Object_Access_Type_Access
     with Pre => Self.Is_Object_Access_Type;

   function To_Procedure_Access_Type
    (Self : access Element'Class)
      return Program.Elements.Procedure_Access_Types
          .Procedure_Access_Type_Access
     with Pre => Self.Is_Procedure_Access_Type;

   function To_Function_Access_Type
    (Self : access Element'Class)
      return Program.Elements.Function_Access_Types.Function_Access_Type_Access
     with Pre => Self.Is_Function_Access_Type;

   function To_Formal_Private_Type_Definition
    (Self : access Element'Class)
      return Program.Elements.Formal_Private_Type_Definitions
          .Formal_Private_Type_Definition_Access
     with Pre => Self.Is_Formal_Private_Type_Definition;

   function To_Formal_Derived_Type_Definition
    (Self : access Element'Class)
      return Program.Elements.Formal_Derived_Type_Definitions
          .Formal_Derived_Type_Definition_Access
     with Pre => Self.Is_Formal_Derived_Type_Definition;

   function To_Formal_Discrete_Type_Definition
    (Self : access Element'Class)
      return Program.Elements.Formal_Discrete_Type_Definitions
          .Formal_Discrete_Type_Definition_Access
     with Pre => Self.Is_Formal_Discrete_Type_Definition;

   function To_Formal_Signed_Integer_Type_Definition
    (Self : access Element'Class)
      return Program.Elements.Formal_Signed_Integer_Type_Definitions
          .Formal_Signed_Integer_Type_Definition_Access
     with Pre => Self.Is_Formal_Signed_Integer_Type_Definition;

   function To_Formal_Modular_Type_Definition
    (Self : access Element'Class)
      return Program.Elements.Formal_Modular_Type_Definitions
          .Formal_Modular_Type_Definition_Access
     with Pre => Self.Is_Formal_Modular_Type_Definition;

   function To_Formal_Floating_Point_Definition
    (Self : access Element'Class)
      return Program.Elements.Formal_Floating_Point_Definitions
          .Formal_Floating_Point_Definition_Access
     with Pre => Self.Is_Formal_Floating_Point_Definition;

   function To_Formal_Ordinary_Fixed_Point_Definition
    (Self : access Element'Class)
      return Program.Elements.Formal_Ordinary_Fixed_Point_Definitions
          .Formal_Ordinary_Fixed_Point_Definition_Access
     with Pre => Self.Is_Formal_Ordinary_Fixed_Point_Definition;

   function To_Formal_Decimal_Fixed_Point_Definition
    (Self : access Element'Class)
      return Program.Elements.Formal_Decimal_Fixed_Point_Definitions
          .Formal_Decimal_Fixed_Point_Definition_Access
     with Pre => Self.Is_Formal_Decimal_Fixed_Point_Definition;

   function To_Formal_Unconstrained_Array_Type
    (Self : access Element'Class)
      return Program.Elements.Formal_Unconstrained_Array_Types
          .Formal_Unconstrained_Array_Type_Access
     with Pre => Self.Is_Formal_Unconstrained_Array_Type;

   function To_Formal_Constrained_Array_Type
    (Self : access Element'Class)
      return Program.Elements.Formal_Constrained_Array_Types
          .Formal_Constrained_Array_Type_Access
     with Pre => Self.Is_Formal_Constrained_Array_Type;

   function To_Formal_Access_Type
    (Self : access Element'Class)
      return Program.Elements.Formal_Access_Types.Formal_Access_Type_Access
     with Pre => Self.Is_Formal_Access_Type;

   function To_Formal_Object_Access_Type
    (Self : access Element'Class)
      return Program.Elements.Formal_Object_Access_Types
          .Formal_Object_Access_Type_Access
     with Pre => Self.Is_Formal_Object_Access_Type;

   function To_Formal_Procedure_Access_Type
    (Self : access Element'Class)
      return Program.Elements.Formal_Procedure_Access_Types
          .Formal_Procedure_Access_Type_Access
     with Pre => Self.Is_Formal_Procedure_Access_Type;

   function To_Formal_Function_Access_Type
    (Self : access Element'Class)
      return Program.Elements.Formal_Function_Access_Types
          .Formal_Function_Access_Type_Access
     with Pre => Self.Is_Formal_Function_Access_Type;

   function To_Formal_Interface_Type
    (Self : access Element'Class)
      return Program.Elements.Formal_Interface_Types
          .Formal_Interface_Type_Access
     with Pre => Self.Is_Formal_Interface_Type;

   function To_Access_Type
    (Self : access Element'Class)
      return Program.Elements.Access_Types.Access_Type_Access
     with Pre => Self.Is_Access_Type;

   function To_Range_Attribute_Reference
    (Self : access Element'Class)
      return Program.Elements.Range_Attribute_References
          .Range_Attribute_Reference_Access
     with Pre => Self.Is_Range_Attribute_Reference;

   function To_Simple_Expression_Range
    (Self : access Element'Class)
      return Program.Elements.Simple_Expression_Ranges
          .Simple_Expression_Range_Access
     with Pre => Self.Is_Simple_Expression_Range;

   function To_Digits_Constraint
    (Self : access Element'Class)
      return Program.Elements.Digits_Constraints.Digits_Constraint_Access
     with Pre => Self.Is_Digits_Constraint;

   function To_Delta_Constraint
    (Self : access Element'Class)
      return Program.Elements.Delta_Constraints.Delta_Constraint_Access
     with Pre => Self.Is_Delta_Constraint;

   function To_Index_Constraint
    (Self : access Element'Class)
      return Program.Elements.Index_Constraints.Index_Constraint_Access
     with Pre => Self.Is_Index_Constraint;

   function To_Discriminant_Constraint
    (Self : access Element'Class)
      return Program.Elements.Discriminant_Constraints
          .Discriminant_Constraint_Access
     with Pre => Self.Is_Discriminant_Constraint;

   function To_Attribute_Definition_Clause
    (Self : access Element'Class)
      return Program.Elements.Attribute_Definition_Clauses
          .Attribute_Definition_Clause_Access
     with Pre => Self.Is_Attribute_Definition_Clause;

   function To_Enumeration_Representation_Clause
    (Self : access Element'Class)
      return Program.Elements.Enumeration_Representation_Clauses
          .Enumeration_Representation_Clause_Access
     with Pre => Self.Is_Enumeration_Representation_Clause;

   function To_Record_Representation_Clause
    (Self : access Element'Class)
      return Program.Elements.Record_Representation_Clauses
          .Record_Representation_Clause_Access
     with Pre => Self.Is_Record_Representation_Clause;

   function To_At_Clause
    (Self : access Element'Class)
      return Program.Elements.At_Clauses.At_Clause_Access
     with Pre => Self.Is_At_Clause;

   function To_Exception_Handler
    (Self : access Element'Class)
      return Program.Elements.Exception_Handlers.Exception_Handler_Access
     with Pre => Self.Is_Exception_Handler;

   not overriding procedure Visit
    (Self    : not null access Element;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class)
     is abstract;

   not overriding function Enclosing_Element
    (Self : Element)
      return Program.Elements.Element_Access is abstract;

   not overriding function Is_Part_Of_Implicit (Self : Element) return Boolean
     is abstract;

   not overriding function Is_Part_Of_Inherited (Self : Element) return Boolean
     is abstract;

   not overriding function Is_Part_Of_Instance (Self : Element) return Boolean
     is abstract;

   function Each_Enclosing_Element
    (Self : not null access Element'Class)
      return Program.Element_Iterators.Enclosing_Element_Iterator;

   function Each_Child
    (Self : not null access Element'Class)
      return Program.Element_Iterators.Child_Iterator;

end Program.Elements;
