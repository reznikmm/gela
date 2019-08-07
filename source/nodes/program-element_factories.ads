--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with System.Storage_Pools.Subpools;
with Program.Elements.Pragmas;
with Program.Elements.Defining_Names;
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
with Program.Elements.Constraints;
with Program.Elements.Component_Definitions;
with Program.Elements.Discrete_Ranges;
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
with Program.Lexical_Elements;
with Program.Element_Vectors;

package Program.Element_Factories is

   type Element_Factory (Subpool : not null System.Storage_Pools.Subpools
       .Subpool_Handle) is tagged limited private;

   not overriding function Create_Pragma
    (Self : Element_Factory;
     Pragma_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Identifiers
         .Identifier_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Arguments           : Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Pragmas.Pragma_Access;

   not overriding function Create_Defining_Identifier
    (Self             : Element_Factory;
     Identifier_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access;

   not overriding function Create_Defining_Character_Literal
    (Self                    : Element_Factory;
     Character_Literal_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Defining_Character_Literals
          .Defining_Character_Literal_Access;

   not overriding function Create_Defining_Operator_Symbol
    (Self                  : Element_Factory;
     Operator_Symbol_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Defining_Operator_Symbols
          .Defining_Operator_Symbol_Access;

   not overriding function Create_Defining_Expanded_Name
    (Self : Element_Factory;
     Prefix    : not null Program.Elements.Expressions.Expression_Access;
     Dot_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Selector  : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access)
      return not null Program.Elements.Defining_Expanded_Names
          .Defining_Expanded_Name_Access;

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
     Aspects           : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Type_Declarations
          .Type_Declaration_Access;

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
     Aspects           : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors       : Program.Elements.Expressions.Expression_Vector_Access;
     With_Token_2      : Program.Lexical_Elements.Lexical_Element_Access;
     Definition        : not null Program.Elements.Task_Definitions
         .Task_Definition_Access;
     Semicolon_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Task_Type_Declarations
          .Task_Type_Declaration_Access;

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
     Aspects           : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors       : Program.Elements.Expressions.Expression_Vector_Access;
     With_Token_2      : Program.Lexical_Elements.Lexical_Element_Access;
     Definition        : not null Program.Elements.Protected_Definitions
         .Protected_Definition_Access;
     Semicolon_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Protected_Type_Declarations
          .Protected_Type_Declaration_Access;

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
     Aspects            : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Subtype_Declarations
          .Subtype_Declaration_Access;

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
     Aspects                   : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Object_Declarations
          .Object_Declaration_Access;

   not overriding function Create_Single_Task_Declaration
    (Self : Element_Factory;
     Task_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors     : Program.Elements.Expressions.Expression_Vector_Access;
     With_Token_2    : Program.Lexical_Elements.Lexical_Element_Access;
     Definition      : not null Program.Elements.Task_Definitions
         .Task_Definition_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Single_Task_Declarations
          .Single_Task_Declaration_Access;

   not overriding function Create_Single_Protected_Declaration
    (Self : Element_Factory;
     Protected_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors     : Program.Elements.Expressions.Expression_Vector_Access;
     With_Token_2    : Program.Lexical_Elements.Lexical_Element_Access;
     Definition      : not null Program.Elements.Protected_Definitions
         .Protected_Definition_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Single_Protected_Declarations
          .Single_Protected_Declaration_Access;

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
          .Number_Declaration_Access;

   not overriding function Create_Enumeration_Literal_Specification
    (Self : Element_Factory;
     Name : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access)
      return not null Program.Elements.Enumeration_Literal_Specifications
          .Enumeration_Literal_Specification_Access;

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
          .Discriminant_Specification_Access;

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
     Aspects            : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Component_Declarations
          .Component_Declaration_Access;

   not overriding function Create_Loop_Parameter_Specification
    (Self : Element_Factory;
     Name          : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     In_Token      : not null Program.Lexical_Elements.Lexical_Element_Access;
     Reverse_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Definition    : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Access)
      return not null Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification_Access;

   not overriding function Create_Generalized_Iterator_Specification
    (Self : Element_Factory;
     Name          : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     In_Token      : not null Program.Lexical_Elements.Lexical_Element_Access;
     Reverse_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Iterator_Name : not null Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification_Access;

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
          .Element_Iterator_Specification_Access;

   not overriding function Create_Procedure_Declaration
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Is_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Abstract_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Procedure_Declarations
          .Procedure_Declaration_Access;

   not overriding function Create_Function_Declaration
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
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
     Aspects             : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Function_Declarations
          .Function_Declaration_Access;

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
          .Parameter_Specification_Access;

   not overriding function Create_Procedure_Body_Declaration
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Declarations        : Program.Element_Vectors.Element_Vector_Access;
     Begin_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements          : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Exception_Handlers  : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name            : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Procedure_Body_Declarations
          .Procedure_Body_Declaration_Access;

   not overriding function Create_Function_Body_Declaration
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token_2         : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Subtype      : not null Program.Elements.Element_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Declarations        : Program.Element_Vectors.Element_Vector_Access;
     Begin_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements          : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Exception_Handlers  : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name            : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Function_Body_Declarations
          .Function_Body_Declaration_Access;

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
          .Return_Object_Specification_Access;

   not overriding function Create_Package_Declaration
    (Self : Element_Factory;
     Package_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     With_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Visible_Declarations : Program.Element_Vectors.Element_Vector_Access;
     Private_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Declarations : Program.Element_Vectors.Element_Vector_Access;
     End_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name             : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Package_Declarations
          .Package_Declaration_Access;

   not overriding function Create_Package_Body_Declaration
    (Self : Element_Factory;
     Package_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Body_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name               : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects            : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Declarations       : Program.Element_Vectors.Element_Vector_Access;
     Begin_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements         : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Exception_Handlers : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name           : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Package_Body_Declarations
          .Package_Body_Declaration_Access;

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
     Aspects         : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Object_Renaming_Declarations
          .Object_Renaming_Declaration_Access;

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
     Aspects           : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Exception_Renaming_Declarations
          .Exception_Renaming_Declaration_Access;

   not overriding function Create_Procedure_Renaming_Declaration
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Renames_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Renamed_Procedure   : Program.Elements.Expressions.Expression_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Procedure_Renaming_Declarations
          .Procedure_Renaming_Declaration_Access;

   not overriding function Create_Function_Renaming_Declaration
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
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
     Aspects             : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Function_Renaming_Declarations
          .Function_Renaming_Declaration_Access;

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
     Aspects         : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Package_Renaming_Declarations
          .Package_Renaming_Declaration_Access;

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
     Aspects         : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Generic_Package_Renaming_Declarations
          .Generic_Package_Renaming_Declaration_Access;

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
     Aspects           : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Generic_Procedure_Renaming_Declarations
          .Generic_Procedure_Renaming_Declaration_Access;

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
     Aspects          : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Generic_Function_Renaming_Declarations
          .Generic_Function_Renaming_Declaration_Access;

   not overriding function Create_Task_Body_Declaration
    (Self : Element_Factory;
     Task_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Body_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name               : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects            : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Declarations       : Program.Element_Vectors.Element_Vector_Access;
     Begin_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements         : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Exception_Handlers : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name           : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Task_Body_Declarations
          .Task_Body_Declaration_Access;

   not overriding function Create_Protected_Body_Declaration
    (Self : Element_Factory;
     Protected_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Body_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     With_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects              : Program.Elements.Aspect_Specifications
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
          .Protected_Body_Declaration_Access;

   not overriding function Create_Entry_Declaration
    (Self : Element_Factory;
     Not_Token               : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Entry_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                    : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Left_Bracket_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Entry_Family_Definition : Program.Elements.Discrete_Ranges
         .Discrete_Range_Access;
     Right_Bracket_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Left_Bracket_Token_2    : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters              : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token_2   : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token              : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects                 : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Entry_Declarations
          .Entry_Declaration_Access;

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
     Parameters            : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token_2 : Program.Lexical_Elements.Lexical_Element_Access;
     When_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Entry_Barrier         : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Token              : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Declarations          : Program.Element_Vectors.Element_Vector_Access;
     Begin_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements            : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Exception_Handlers    : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name              : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Entry_Body_Declarations
          .Entry_Body_Declaration_Access;

   not overriding function Create_Entry_Index_Specification
    (Self : Element_Factory;
     For_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     In_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Entry_Index_Subtype : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Access)
      return not null Program.Elements.Entry_Index_Specifications
          .Entry_Index_Specification_Access;

   not overriding function Create_Procedure_Body_Stub
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Is_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Separate_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Procedure_Body_Stubs
          .Procedure_Body_Stub_Access;

   not overriding function Create_Function_Body_Stub
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Overriding_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
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
     Aspects             : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Function_Body_Stubs
          .Function_Body_Stub_Access;

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
     Aspects         : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Package_Body_Stubs
          .Package_Body_Stub_Access;

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
     Aspects         : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Task_Body_Stubs.Task_Body_Stub_Access;

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
     Aspects         : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Protected_Body_Stubs
          .Protected_Body_Stub_Access;

   not overriding function Create_Exception_Declaration
    (Self : Element_Factory;
     Names           : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Exception_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Exception_Declarations
          .Exception_Declaration_Access;

   not overriding function Create_Choice_Parameter_Specification
    (Self : Element_Factory;
     Name        : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Choice_Parameter_Specifications
          .Choice_Parameter_Specification_Access;

   not overriding function Create_Generic_Package_Declaration
    (Self : Element_Factory;
     Generic_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Formal_Parameters    : Program.Element_Vectors.Element_Vector_Access;
     Package_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     With_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Visible_Declarations : Program.Element_Vectors.Element_Vector_Access;
     Private_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Declarations : Program.Element_Vectors.Element_Vector_Access;
     End_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name             : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Generic_Package_Declarations
          .Generic_Package_Declaration_Access;

   not overriding function Create_Generic_Procedure_Declaration
    (Self : Element_Factory;
     Generic_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Formal_Parameters   : Program.Element_Vectors.Element_Vector_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Generic_Procedure_Declarations
          .Generic_Procedure_Declaration_Access;

   not overriding function Create_Generic_Function_Declaration
    (Self : Element_Factory;
     Generic_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Formal_Parameters   : Program.Element_Vectors.Element_Vector_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Subtype      : not null Program.Elements.Element_Access;
     With_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Generic_Function_Declarations
          .Generic_Function_Declaration_Access;

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
     Parameters           : Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Package_Instantiations
          .Package_Instantiation_Access;

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
     Parameters             : Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token             : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects                : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Procedure_Instantiations
          .Procedure_Instantiation_Access;

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
     Parameters            : Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects               : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Function_Instantiations
          .Function_Instantiation_Access;

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
     Aspects            : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Formal_Object_Declarations
          .Formal_Object_Declaration_Access;

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
     Aspects           : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Formal_Type_Declarations
          .Formal_Type_Declaration_Access;

   not overriding function Create_Formal_Procedure_Declaration
    (Self : Element_Factory;
     With_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Is_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Abstract_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Subprogram_Default  : Program.Elements.Expressions.Expression_Access;
     Box_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token_2        : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects             : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Formal_Procedure_Declarations
          .Formal_Procedure_Declaration_Access;

   not overriding function Create_Formal_Function_Declaration
    (Self : Element_Factory;
     With_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
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
     Aspects             : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Formal_Function_Declarations
          .Formal_Function_Declaration_Access;

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
     Parameters           : Program.Elements.Formal_Package_Associations
         .Formal_Package_Association_Vector_Access;
     Right_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token_2         : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Formal_Package_Declarations
          .Formal_Package_Declaration_Access;

   not overriding function Create_Subtype_Indication
    (Self : Element_Factory;
     Not_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Subtype_Mark : not null Program.Elements.Expressions.Expression_Access;
     Constraint   : Program.Elements.Constraints.Constraint_Access)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access;

   not overriding function Create_Component_Definition
    (Self : Element_Factory;
     Aliased_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Subtype_Indication : not null Program.Elements.Element_Access)
      return not null Program.Elements.Component_Definitions
          .Component_Definition_Access;

   not overriding function Create_Discrete_Subtype_Indication
    (Self : Element_Factory;
     Subtype_Mark                   : not null Program.Elements.Expressions
         .Expression_Access;
     Constraint                     : Program.Elements.Constraints
         .Constraint_Access;
     Is_Discrete_Subtype_Definition : Boolean := False)
      return not null Program.Elements.Discrete_Subtype_Indications
          .Discrete_Subtype_Indication_Access;

   not overriding function Create_Discrete_Range_Attribute_Reference
    (Self : Element_Factory;
     Range_Attribute                : not null Program.Elements
         .Attribute_References.Attribute_Reference_Access;
     Is_Discrete_Subtype_Definition : Boolean := False)
      return not null Program.Elements.Discrete_Range_Attribute_References
          .Discrete_Range_Attribute_Reference_Access;

   not overriding function Create_Discrete_Simple_Expression_Range
    (Self : Element_Factory;
     Lower_Bound                    : not null Program.Elements.Expressions
         .Expression_Access;
     Double_Dot_Token               : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Upper_Bound                    : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Discrete_Subtype_Definition : Boolean := False)
      return not null Program.Elements.Discrete_Simple_Expression_Ranges
          .Discrete_Simple_Expression_Range_Access;

   not overriding function Create_Unknown_Discriminant_Part
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Box_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Unknown_Discriminant_Parts
          .Unknown_Discriminant_Part_Access;

   not overriding function Create_Known_Discriminant_Part
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Discriminants       : Program.Elements.Discriminant_Specifications
         .Discriminant_Specification_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Known_Discriminant_Parts
          .Known_Discriminant_Part_Access;

   not overriding function Create_Record_Definition
    (Self : Element_Factory;
     Record_Token   : not null Program.Lexical_Elements.Lexical_Element_Access;
     Components     : not null Program.Element_Vectors.Element_Vector_Access;
     End_Token      : not null Program.Lexical_Elements.Lexical_Element_Access;
     Record_Token_2 : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Record_Definitions
          .Record_Definition_Access;

   not overriding function Create_Null_Component
    (Self : Element_Factory;
     Null_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Null_Components.Null_Component_Access;

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
      return not null Program.Elements.Variant_Parts.Variant_Part_Access;

   not overriding function Create_Variant
    (Self : Element_Factory;
     When_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Choices     : not null Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Components  : not null Program.Element_Vectors.Element_Vector_Access)
      return not null Program.Elements.Variants.Variant_Access;

   not overriding function Create_Others_Choice
    (Self         : Element_Factory;
     Others_Token : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Others_Choices.Others_Choice_Access;

   not overriding function Create_Anonymous_Access_To_Object
    (Self : Element_Factory;
     Not_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Access_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     All_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Constant_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Subtype_Indication : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access)
      return not null Program.Elements.Anonymous_Access_To_Objects
          .Anonymous_Access_To_Object_Access;

   not overriding function Create_Anonymous_Access_To_Procedure
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Access_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Protected_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Anonymous_Access_To_Procedures
          .Anonymous_Access_To_Procedure_Access;

   not overriding function Create_Anonymous_Access_To_Function
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Access_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Protected_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token_2         : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token_2        : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Subtype      : not null Program.Elements.Element_Access)
      return not null Program.Elements.Anonymous_Access_To_Functions
          .Anonymous_Access_To_Function_Access;

   not overriding function Create_Private_Type_Definition
    (Self : Element_Factory;
     Abstract_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Tagged_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Limited_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Token  : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Private_Type_Definitions
          .Private_Type_Definition_Access;

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
     Progenitors        : Program.Elements.Expressions
         .Expression_Vector_Access;
     With_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Private_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Private_Extension_Definitions
          .Private_Extension_Definition_Access;

   not overriding function Create_Incomplete_Type_Definition
    (Self         : Element_Factory;
     Tagged_Token : Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Incomplete_Type_Definitions
          .Incomplete_Type_Definition_Access;

   not overriding function Create_Task_Definition
    (Self : Element_Factory;
     Visible_Declarations : Program.Element_Vectors.Element_Vector_Access;
     Private_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Private_Declarations : Program.Element_Vectors.Element_Vector_Access;
     End_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access)
      return not null Program.Elements.Task_Definitions.Task_Definition_Access;

   not overriding function Create_Protected_Definition
    (Self : Element_Factory;
     Visible_Declarations : Program.Element_Vectors.Element_Vector_Access;
     Private_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Private_Declarations : Program.Element_Vectors.Element_Vector_Access;
     End_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name             : Program.Elements.Identifiers.Identifier_Access)
      return not null Program.Elements.Protected_Definitions
          .Protected_Definition_Access;

   not overriding function Create_Aspect_Specification
    (Self : Element_Factory;
     Aspect_Mark       : not null Program.Elements.Expressions
         .Expression_Access;
     Arrow_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Aspect_Definition : not null Program.Elements.Expressions
         .Expression_Access)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Access;

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
          .Real_Range_Specification_Access;

   not overriding function Create_Numeric_Literal
    (Self                  : Element_Factory;
     Numeric_Literal_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Numeric_Literals.Numeric_Literal_Access;

   not overriding function Create_String_Literal
    (Self                 : Element_Factory;
     String_Literal_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.String_Literals.String_Literal_Access;

   not overriding function Create_Identifier
    (Self             : Element_Factory;
     Identifier_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Identifiers.Identifier_Access;

   not overriding function Create_Operator_Symbol
    (Self                  : Element_Factory;
     Operator_Symbol_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Operator_Symbols.Operator_Symbol_Access;

   not overriding function Create_Character_Literal
    (Self                    : Element_Factory;
     Character_Literal_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Character_Literals
          .Character_Literal_Access;

   not overriding function Create_Explicit_Dereference
    (Self : Element_Factory;
     Prefix    : not null Program.Elements.Expressions.Expression_Access;
     Dot_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     All_Token : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Explicit_Dereferences
          .Explicit_Dereference_Access;

   not overriding function Create_Function_Call
    (Self : Element_Factory;
     Prefix              : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Function_Calls.Function_Call_Access;

   not overriding function Create_Indexed_Component
    (Self : Element_Factory;
     Prefix              : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expressions         : Program.Elements.Expressions
         .Expression_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Indexed_Components
          .Indexed_Component_Access;

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
      return not null Program.Elements.Slices.Slice_Access;

   not overriding function Create_Selected_Component
    (Self : Element_Factory;
     Prefix    : not null Program.Elements.Expressions.Expression_Access;
     Dot_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Selector  : not null Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Selected_Components
          .Selected_Component_Access;

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
          .Attribute_Reference_Access;

   not overriding function Create_Record_Aggregate
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Components          : Program.Elements.Record_Component_Associations
         .Record_Component_Association_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Record_Aggregates
          .Record_Aggregate_Access;

   not overriding function Create_Extension_Aggregate
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Ancestor            : not null Program.Elements.Expressions
         .Expression_Access;
     With_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Components          : Program.Elements.Record_Component_Associations
         .Record_Component_Association_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Extension_Aggregates
          .Extension_Aggregate_Access;

   not overriding function Create_Array_Aggregate
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Components          : Program.Elements.Array_Component_Associations
         .Array_Component_Association_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Array_Aggregates.Array_Aggregate_Access;

   not overriding function Create_Short_Circuit_Operation
    (Self : Element_Factory;
     Left       : not null Program.Elements.Expressions.Expression_Access;
     And_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Then_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Or_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Else_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Right      : not null Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Short_Circuit_Operations
          .Short_Circuit_Operation_Access;

   not overriding function Create_Membership_Test
    (Self : Element_Factory;
     Expression : not null Program.Elements.Expressions.Expression_Access;
     Not_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     In_Token   : not null Program.Lexical_Elements.Lexical_Element_Access;
     Choices    : not null Program.Element_Vectors.Element_Vector_Access)
      return not null Program.Elements.Membership_Tests.Membership_Test_Access;

   not overriding function Create_Null_Literal
    (Self               : Element_Factory;
     Null_Literal_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Null_Literals.Null_Literal_Access;

   not overriding function Create_Parenthesized_Expression
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression          : not null Program.Elements.Expressions
         .Expression_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Parenthesized_Expressions
          .Parenthesized_Expression_Access;

   not overriding function Create_Raise_Expression
    (Self : Element_Factory;
     Raise_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Exception_Name     : not null Program.Elements.Expressions
         .Expression_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Associated_Message : Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Raise_Expressions
          .Raise_Expression_Access;

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
      return not null Program.Elements.Type_Conversions.Type_Conversion_Access;

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
          .Qualified_Expression_Access;

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
      return not null Program.Elements.Allocators.Allocator_Access;

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
      return not null Program.Elements.Case_Expressions.Case_Expression_Access;

   not overriding function Create_If_Expression
    (Self : Element_Factory;
     If_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Condition       : not null Program.Elements.Expressions.Expression_Access;
     Then_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Then_Expression : not null Program.Elements.Expressions.Expression_Access;
     Elsif_Paths     : Program.Elements.Elsif_Paths.Elsif_Path_Vector_Access;
     Else_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Else_Expression : Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.If_Expressions.If_Expression_Access;

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
          .Quantified_Expression_Access;

   not overriding function Create_Discriminant_Association
    (Self : Element_Factory;
     Selector_Names : Program.Elements.Identifiers.Identifier_Vector_Access;
     Arrow_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Expression     : not null Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Discriminant_Associations
          .Discriminant_Association_Access;

   not overriding function Create_Record_Component_Association
    (Self : Element_Factory;
     Choices     : Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Expression  : Program.Elements.Expressions.Expression_Access;
     Box_Token   : Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Record_Component_Associations
          .Record_Component_Association_Access;

   not overriding function Create_Array_Component_Association
    (Self : Element_Factory;
     Choices     : Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Expression  : Program.Elements.Expressions.Expression_Access;
     Box_Token   : Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Array_Component_Associations
          .Array_Component_Association_Access;

   not overriding function Create_Parameter_Association
    (Self : Element_Factory;
     Formal_Parameter : Program.Elements.Expressions.Expression_Access;
     Arrow_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Actual_Parameter : not null Program.Elements.Expressions
         .Expression_Access)
      return not null Program.Elements.Parameter_Associations
          .Parameter_Association_Access;

   not overriding function Create_Formal_Package_Association
    (Self : Element_Factory;
     Formal_Parameter : Program.Elements.Expressions.Expression_Access;
     Arrow_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Actual_Parameter : Program.Elements.Expressions.Expression_Access;
     Box_Token        : Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Formal_Package_Associations
          .Formal_Package_Association_Access;

   not overriding function Create_Null_Statement
    (Self : Element_Factory;
     Null_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Null_Statements.Null_Statement_Access;

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
          .Assignment_Statement_Access;

   not overriding function Create_If_Statement
    (Self : Element_Factory;
     If_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Condition       : not null Program.Elements.Expressions.Expression_Access;
     Then_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Then_Statements : not null Program.Element_Vectors.Element_Vector_Access;
     Elsif_Paths     : Program.Elements.Elsif_Paths.Elsif_Path_Vector_Access;
     Else_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Else_Statements : Program.Element_Vectors.Element_Vector_Access;
     End_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     If_Token_2      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.If_Statements.If_Statement_Access;

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
      return not null Program.Elements.Case_Statements.Case_Statement_Access;

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
      return not null Program.Elements.Loop_Statements.Loop_Statement_Access;

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
          .While_Loop_Statement_Access;

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
          .For_Loop_Statement_Access;

   not overriding function Create_Block_Statement
    (Self : Element_Factory;
     Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token              : Program.Lexical_Elements
         .Lexical_Element_Access;
     Declare_Token            : Program.Lexical_Elements
         .Lexical_Element_Access;
     Declarations             : Program.Element_Vectors.Element_Vector_Access;
     Begin_Token              : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     Exception_Token          : Program.Lexical_Elements
         .Lexical_Element_Access;
     Exception_Handlers       : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token                : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Block_Statements.Block_Statement_Access;

   not overriding function Create_Exit_Statement
    (Self : Element_Factory;
     Exit_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Exit_Loop_Name  : Program.Elements.Expressions.Expression_Access;
     When_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Condition       : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Exit_Statements.Exit_Statement_Access;

   not overriding function Create_Goto_Statement
    (Self : Element_Factory;
     Goto_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Goto_Label      : not null Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Goto_Statements.Goto_Statement_Access;

   not overriding function Create_Call_Statement
    (Self : Element_Factory;
     Called_Name         : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Semicolon_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Call_Statements.Call_Statement_Access;

   not overriding function Create_Simple_Return_Statement
    (Self : Element_Factory;
     Return_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression      : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Simple_Return_Statements
          .Simple_Return_Statement_Access;

   not overriding function Create_Extended_Return_Statement
    (Self : Element_Factory;
     Return_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Return_Object      : not null Program.Elements
         .Return_Object_Specifications.Return_Object_Specification_Access;
     Do_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Statements         : Program.Element_Vectors.Element_Vector_Access;
     Exception_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Exception_Handlers : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token_2     : Program.Lexical_Elements.Lexical_Element_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Extended_Return_Statements
          .Extended_Return_Statement_Access;

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
     Parameters               : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token_2    : Program.Lexical_Elements
         .Lexical_Element_Access;
     Do_Token                 : Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements               : Program.Element_Vectors.Element_Vector_Access;
     Exception_Token          : Program.Lexical_Elements
         .Lexical_Element_Access;
     Exception_Handlers       : Program.Elements.Exception_Handlers
         .Exception_Handler_Vector_Access;
     End_Token                : Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Accept_Statements
          .Accept_Statement_Access;

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
          .Requeue_Statement_Access;

   not overriding function Create_Delay_Statement
    (Self : Element_Factory;
     Delay_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Until_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression      : not null Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Delay_Statements.Delay_Statement_Access;

   not overriding function Create_Terminate_Alternative_Statement
    (Self : Element_Factory;
     Terminate_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Terminate_Alternative_Statements
          .Terminate_Alternative_Statement_Access;

   not overriding function Create_Select_Statement
    (Self : Element_Factory;
     Select_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Paths                 : not null Program.Elements.Select_Paths
         .Select_Path_Vector_Access;
     Then_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Abort_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Then_Abort_Statements : Program.Element_Vectors.Element_Vector_Access;
     Else_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Else_Statements       : Program.Element_Vectors.Element_Vector_Access;
     End_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Select_Token_2        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Select_Statements
          .Select_Statement_Access;

   not overriding function Create_Abort_Statement
    (Self : Element_Factory;
     Abort_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Aborted_Tasks   : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Abort_Statements.Abort_Statement_Access;

   not overriding function Create_Raise_Statement
    (Self : Element_Factory;
     Raise_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Raised_Exception   : Program.Elements.Expressions.Expression_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Associated_Message : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Raise_Statements.Raise_Statement_Access;

   not overriding function Create_Code_Statement
    (Self : Element_Factory;
     Expression      : not null Program.Elements.Qualified_Expressions
         .Qualified_Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Code_Statements.Code_Statement_Access;

   not overriding function Create_Elsif_Path
    (Self : Element_Factory;
     Elsif_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Condition   : not null Program.Elements.Expressions.Expression_Access;
     Then_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Statements  : not null Program.Element_Vectors.Element_Vector_Access)
      return not null Program.Elements.Elsif_Paths.Elsif_Path_Access;

   not overriding function Create_Case_Path
    (Self : Element_Factory;
     When_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Choices     : not null Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Statements  : not null Program.Element_Vectors.Element_Vector_Access)
      return not null Program.Elements.Case_Paths.Case_Path_Access;

   not overriding function Create_Select_Path
    (Self : Element_Factory;
     When_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Guard       : Program.Elements.Expressions.Expression_Access;
     Arrow_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Statements  : not null Program.Element_Vectors.Element_Vector_Access)
      return not null Program.Elements.Select_Paths.Select_Path_Access;

   not overriding function Create_Case_Expression_Path
    (Self : Element_Factory;
     When_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Choices     : not null Program.Element_Vectors.Element_Vector_Access;
     Arrow_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Expression  : not null Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Case_Expression_Paths
          .Case_Expression_Path_Access;

   not overriding function Create_Elsif_Expression_Path
    (Self : Element_Factory;
     Elsif_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Condition   : not null Program.Elements.Expressions.Expression_Access;
     Then_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Expression  : not null Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Elsif_Expression_Paths
          .Elsif_Expression_Path_Access;

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
      return not null Program.Elements.Use_Clauses.Use_Clause_Access;

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
      return not null Program.Elements.With_Clauses.With_Clause_Access;

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
          .Component_Clause_Access;

   not overriding function Create_Derived_Type
    (Self : Element_Factory;
     Abstract_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Limited_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     New_Token      : not null Program.Lexical_Elements.Lexical_Element_Access;
     Parent         : not null Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Derived_Types.Derived_Type_Access;

   not overriding function Create_Derived_Record_Extension
    (Self : Element_Factory;
     Abstract_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Limited_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     New_Token         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Parent            : not null Program.Elements.Expressions
         .Expression_Access;
     And_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors       : Program.Elements.Expressions.Expression_Vector_Access;
     With_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Record_Definition : not null Program.Elements.Definitions
         .Definition_Access)
      return not null Program.Elements.Derived_Record_Extensions
          .Derived_Record_Extension_Access;

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
          .Enumeration_Type_Access;

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
          .Signed_Integer_Type_Access;

   not overriding function Create_Modular_Type
    (Self : Element_Factory;
     Mod_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Modulus   : not null Program.Elements.Expressions.Expression_Access)
      return not null Program.Elements.Modular_Types.Modular_Type_Access;

   not overriding function Create_Root_Type
    (Self : Element_Factory)
      return not null Program.Elements.Root_Types.Root_Type_Access;

   not overriding function Create_Floating_Point_Type
    (Self : Element_Factory;
     Digits_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Digits_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range        : Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access)
      return not null Program.Elements.Floating_Point_Types
          .Floating_Point_Type_Access;

   not overriding function Create_Ordinary_Fixed_Point_Type
    (Self : Element_Factory;
     Delta_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Delta_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range       : not null Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access)
      return not null Program.Elements.Ordinary_Fixed_Point_Types
          .Ordinary_Fixed_Point_Type_Access;

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
          .Decimal_Fixed_Point_Type_Access;

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
          .Unconstrained_Array_Type_Access;

   not overriding function Create_Constrained_Array_Type
    (Self : Element_Factory;
     Array_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Index_Subtypes       : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Vector_Access;
     Right_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Of_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Component_Definition : not null Program.Elements.Component_Definitions
         .Component_Definition_Access)
      return not null Program.Elements.Constrained_Array_Types
          .Constrained_Array_Type_Access;

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
      return not null Program.Elements.Record_Types.Record_Type_Access;

   not overriding function Create_Interface_Type
    (Self : Element_Factory;
     Limited_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Task_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Protected_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Synchronized_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Interface_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     And_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors        : Program.Elements.Expressions
         .Expression_Vector_Access)
      return not null Program.Elements.Interface_Types.Interface_Type_Access;

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
          .Object_Access_Type_Access;

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
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Procedure_Access_Types
          .Procedure_Access_Type_Access;

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
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token_2         : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token_2        : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Subtype      : not null Program.Elements.Element_Access)
      return not null Program.Elements.Function_Access_Types
          .Function_Access_Type_Access;

   not overriding function Create_Formal_Private_Type_Definition
    (Self : Element_Factory;
     Abstract_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Tagged_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Limited_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Token  : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Formal_Private_Type_Definitions
          .Formal_Private_Type_Definition_Access;

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
     Progenitors        : Program.Elements.Expressions
         .Expression_Vector_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Token      : Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Formal_Derived_Type_Definitions
          .Formal_Derived_Type_Definition_Access;

   not overriding function Create_Formal_Discrete_Type_Definition
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Box_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Formal_Discrete_Type_Definitions
          .Formal_Discrete_Type_Definition_Access;

   not overriding function Create_Formal_Signed_Integer_Type_Definition
    (Self : Element_Factory;
     Range_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Box_Token   : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Formal_Signed_Integer_Type_Definitions
          .Formal_Signed_Integer_Type_Definition_Access;

   not overriding function Create_Formal_Modular_Type_Definition
    (Self : Element_Factory;
     Mod_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Box_Token : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Formal_Modular_Type_Definitions
          .Formal_Modular_Type_Definition_Access;

   not overriding function Create_Formal_Floating_Point_Definition
    (Self : Element_Factory;
     Digits_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Box_Token    : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Formal_Floating_Point_Definitions
          .Formal_Floating_Point_Definition_Access;

   not overriding function Create_Formal_Ordinary_Fixed_Point_Definition
    (Self : Element_Factory;
     Delta_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Box_Token   : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Formal_Ordinary_Fixed_Point_Definitions
          .Formal_Ordinary_Fixed_Point_Definition_Access;

   not overriding function Create_Formal_Decimal_Fixed_Point_Definition
    (Self : Element_Factory;
     Delta_Token  : not null Program.Lexical_Elements.Lexical_Element_Access;
     Box_Token    : not null Program.Lexical_Elements.Lexical_Element_Access;
     Digits_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Box_Token_2  : not null Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Formal_Decimal_Fixed_Point_Definitions
          .Formal_Decimal_Fixed_Point_Definition_Access;

   not overriding function Create_Formal_Unconstrained_Array_Type
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
      return not null Program.Elements.Formal_Unconstrained_Array_Types
          .Formal_Unconstrained_Array_Type_Access;

   not overriding function Create_Formal_Constrained_Array_Type
    (Self : Element_Factory;
     Array_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Index_Subtypes       : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Vector_Access;
     Right_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Of_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Component_Definition : not null Program.Elements.Component_Definitions
         .Component_Definition_Access)
      return not null Program.Elements.Formal_Constrained_Array_Types
          .Formal_Constrained_Array_Type_Access;

   not overriding function Create_Formal_Object_Access_Type
    (Self : Element_Factory;
     Not_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Access_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     All_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Constant_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Subtype_Indication : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access)
      return not null Program.Elements.Formal_Object_Access_Types
          .Formal_Object_Access_Type_Access;

   not overriding function Create_Formal_Procedure_Access_Type
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Access_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Protected_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Procedure_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access)
      return not null Program.Elements.Formal_Procedure_Access_Types
          .Formal_Procedure_Access_Type_Access;

   not overriding function Create_Formal_Function_Access_Type
    (Self : Element_Factory;
     Not_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Access_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Protected_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Function_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Elements.Parameter_Specifications
         .Parameter_Specification_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Return_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Not_Token_2         : Program.Lexical_Elements.Lexical_Element_Access;
     Null_Token_2        : Program.Lexical_Elements.Lexical_Element_Access;
     Result_Subtype      : not null Program.Elements.Element_Access)
      return not null Program.Elements.Formal_Function_Access_Types
          .Formal_Function_Access_Type_Access;

   not overriding function Create_Formal_Interface_Type
    (Self : Element_Factory;
     Limited_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Task_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Protected_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     Synchronized_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Interface_Token    : Program.Lexical_Elements.Lexical_Element_Access;
     And_Token          : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors        : Program.Elements.Expressions
         .Expression_Vector_Access)
      return not null Program.Elements.Formal_Interface_Types
          .Formal_Interface_Type_Access;

   not overriding function Create_Range_Attribute_Reference
    (Self            : Element_Factory;
     Range_Attribute : not null Program.Elements.Attribute_References
         .Attribute_Reference_Access)
      return not null Program.Elements.Range_Attribute_References
          .Range_Attribute_Reference_Access;

   not overriding function Create_Simple_Expression_Range
    (Self : Element_Factory;
     Lower_Bound      : not null Program.Elements.Expressions
         .Expression_Access;
     Double_Dot_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Upper_Bound      : not null Program.Elements.Expressions
         .Expression_Access)
      return not null Program.Elements.Simple_Expression_Ranges
          .Simple_Expression_Range_Access;

   not overriding function Create_Digits_Constraint
    (Self : Element_Factory;
     Digits_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Digits_Expression     : not null Program.Elements.Expressions
         .Expression_Access;
     Range_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Real_Range_Constraint : Program.Elements.Constraints.Constraint_Access)
      return not null Program.Elements.Digits_Constraints
          .Digits_Constraint_Access;

   not overriding function Create_Delta_Constraint
    (Self : Element_Factory;
     Delta_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Delta_Expression      : not null Program.Elements.Expressions
         .Expression_Access;
     Range_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Real_Range_Constraint : Program.Elements.Constraints.Constraint_Access)
      return not null Program.Elements.Delta_Constraints
          .Delta_Constraint_Access;

   not overriding function Create_Index_Constraint
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Ranges              : not null Program.Elements.Discrete_Ranges
         .Discrete_Range_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Index_Constraints
          .Index_Constraint_Access;

   not overriding function Create_Discriminant_Constraint
    (Self : Element_Factory;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Discriminants       : not null Program.Elements.Discriminant_Associations
         .Discriminant_Association_Vector_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return not null Program.Elements.Discriminant_Constraints
          .Discriminant_Constraint_Access;

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
          .Attribute_Definition_Clause_Access;

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
          .Enumeration_Representation_Clause_Access;

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
          .Record_Representation_Clause_Access;

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
      return not null Program.Elements.At_Clauses.At_Clause_Access;

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
          .Exception_Handler_Access;
private

   type Element_Factory (Subpool : not null System.Storage_Pools.Subpools
       .Subpool_Handle) is tagged limited null record;

end Program.Element_Factories;
