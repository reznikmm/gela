with Gela.Compilations;
with Gela.Lexical_Types;

with Gela.Element_Visiters;
with Gela.Elements.Abort_Statements;
with Gela.Elements.Accept_Statements;
with Gela.Elements.Access_To_Function_Definitions;
with Gela.Elements.Access_To_Object_Definitions;
with Gela.Elements.Access_To_Procedure_Definitions;
with Gela.Elements.Allocators;
with Gela.Elements.Anonymous_Access_To_Function_Definitions;
with Gela.Elements.Anonymous_Access_To_Object_Definitions;
with Gela.Elements.Anonymous_Access_To_Procedure_Definitions;
with Gela.Elements.Aspect_Specifications;
with Gela.Elements.Assignment_Statements;
with Gela.Elements.Association_Lists;
with Gela.Elements.Associations;
with Gela.Elements.Asynchronous_Selects;
with Gela.Elements.At_Clauses;
with Gela.Elements.Attribute_Definition_Clauses;
with Gela.Elements.Attribute_References;
with Gela.Elements.Block_Statements;
with Gela.Elements.Boxes;
with Gela.Elements.Case_Expression_Paths;
with Gela.Elements.Case_Expressions;
with Gela.Elements.Case_Paths;
with Gela.Elements.Case_Statements;
with Gela.Elements.Character_Literals;
with Gela.Elements.Choice_Parameter_Specifications;
with Gela.Elements.Compilation_Unit_Bodies;
with Gela.Elements.Compilation_Unit_Declarations;
with Gela.Elements.Compilations;
with Gela.Elements.Component_Clauses;
with Gela.Elements.Component_Declarations;
with Gela.Elements.Component_Definitions;
with Gela.Elements.Composite_Constraints;
with Gela.Elements.Composite_Subtype_Indications;
with Gela.Elements.Constrained_Array_Definitions;
with Gela.Elements.Decimal_Fixed_Point_Definitions;
with Gela.Elements.Defining_Character_Literals;
with Gela.Elements.Defining_Enumeration_Literals;
with Gela.Elements.Defining_Expanded_Unit_Names;
with Gela.Elements.Defining_Identifiers;
with Gela.Elements.Defining_Names;
with Gela.Elements.Defining_Operator_Symbols;
with Gela.Elements.Delay_Statements;
with Gela.Elements.Delta_Constraints;
with Gela.Elements.Derived_Record_Definitions;
with Gela.Elements.Derived_Type_Definitions;
with Gela.Elements.Digits_Constraints;
with Gela.Elements.Discrete_Range_Attribute_References;
with Gela.Elements.Discrete_Simple_Expression_Ranges;
with Gela.Elements.Discrete_Subtype_Indication_Drs;
with Gela.Elements.Discrete_Subtype_Indications;
with Gela.Elements.Discriminant_Specifications;
with Gela.Elements.Element_Iterator_Specifications;
with Gela.Elements.Else_Expression_Paths;
with Gela.Elements.Else_Paths;
with Gela.Elements.Elsif_Expression_Paths;
with Gela.Elements.Elsif_Paths;
with Gela.Elements.Entry_Bodies;
with Gela.Elements.Entry_Declarations;
with Gela.Elements.Entry_Index_Specifications;
with Gela.Elements.Enumeration_Literal_Specifications;
with Gela.Elements.Enumeration_Type_Definitions;
with Gela.Elements.Exception_Declarations;
with Gela.Elements.Exception_Handlers;
with Gela.Elements.Exception_Renaming_Declarations;
with Gela.Elements.Exit_Statements;
with Gela.Elements.Explicit_Dereferences;
with Gela.Elements.Extended_Return_Statements;
with Gela.Elements.Extension_Aggregates;
with Gela.Elements.Floating_Point_Definitions;
with Gela.Elements.For_Loop_Statements;
with Gela.Elements.Formal_Access_To_Function_Definitions;
with Gela.Elements.Formal_Access_To_Object_Definitions;
with Gela.Elements.Formal_Access_To_Procedure_Definitions;
with Gela.Elements.Formal_Constrained_Array_Definitions;
with Gela.Elements.Formal_Decimal_Fixed_Point_Definitions;
with Gela.Elements.Formal_Derived_Type_Definitions;
with Gela.Elements.Formal_Discrete_Type_Definitions;
with Gela.Elements.Formal_Floating_Point_Definitions;
with Gela.Elements.Formal_Function_Declarations;
with Gela.Elements.Formal_Incomplete_Type_Declarations;
with Gela.Elements.Formal_Interface_Type_Definitions;
with Gela.Elements.Formal_Modular_Type_Definitions;
with Gela.Elements.Formal_Object_Declarations;
with Gela.Elements.Formal_Ordinary_Fixed_Point_Definitions;
with Gela.Elements.Formal_Package_Declarations;
with Gela.Elements.Formal_Private_Type_Definitions;
with Gela.Elements.Formal_Procedure_Declarations;
with Gela.Elements.Formal_Signed_Integer_Type_Definitions;
with Gela.Elements.Formal_Type_Declarations;
with Gela.Elements.Formal_Unconstrained_Array_Definitions;
with Gela.Elements.Full_Type_Declarations;
with Gela.Elements.Function_Bodies;
with Gela.Elements.Function_Calls;
with Gela.Elements.Function_Declarations;
with Gela.Elements.Function_Instantiations;
with Gela.Elements.Generalized_Iterator_Specifications;
with Gela.Elements.Generic_Associations;
with Gela.Elements.Generic_Function_Declarations;
with Gela.Elements.Generic_Function_Renamings;
with Gela.Elements.Generic_Package_Declarations;
with Gela.Elements.Generic_Package_Renamings;
with Gela.Elements.Generic_Procedure_Declarations;
with Gela.Elements.Generic_Procedure_Renamings;
with Gela.Elements.Goto_Statements;
with Gela.Elements.Identifiers;
with Gela.Elements.If_Expression_Paths;
with Gela.Elements.If_Expressions;
with Gela.Elements.If_Paths;
with Gela.Elements.If_Statements;
with Gela.Elements.Incomplete_Type_Declarations;
with Gela.Elements.Incomplete_Type_Definitions;
with Gela.Elements.Interface_Type_Definitions;
with Gela.Elements.Known_Discriminant_Parts;
with Gela.Elements.Label_Decorators;
with Gela.Elements.Loop_Parameter_Specifications;
with Gela.Elements.Loop_Statements;
with Gela.Elements.Membership_Tests;
with Gela.Elements.Modular_Type_Definitions;
with Gela.Elements.Names;
with Gela.Elements.Null_Components;
with Gela.Elements.Null_Literals;
with Gela.Elements.Null_Record_Definitions;
with Gela.Elements.Null_Statements;
with Gela.Elements.Number_Declarations;
with Gela.Elements.Numeric_Literals;
with Gela.Elements.Object_Declarations;
with Gela.Elements.Object_Renaming_Declarations;
with Gela.Elements.Operator_Symbols;
with Gela.Elements.Ordinary_Fixed_Point_Definitions;
with Gela.Elements.Others_Choices;
with Gela.Elements.Package_Bodies;
with Gela.Elements.Package_Body_Stubs;
with Gela.Elements.Package_Declarations;
with Gela.Elements.Package_Instantiations;
with Gela.Elements.Package_Renaming_Declarations;
with Gela.Elements.Parameter_Associations;
with Gela.Elements.Parameter_Specifications;
with Gela.Elements.Parenthesized_Expressions;
with Gela.Elements.Pragma_Argument_Associations;
with Gela.Elements.Pragma_Nodes;
with Gela.Elements.Private_Extension_Declarations;
with Gela.Elements.Private_Extension_Definitions;
with Gela.Elements.Private_Type_Declarations;
with Gela.Elements.Private_Type_Definitions;
with Gela.Elements.Procedure_Bodies;
with Gela.Elements.Procedure_Call_Statements;
with Gela.Elements.Procedure_Declarations;
with Gela.Elements.Procedure_Instantiations;
with Gela.Elements.Protected_Bodies;
with Gela.Elements.Protected_Body_Stubs;
with Gela.Elements.Protected_Definitions;
with Gela.Elements.Protected_Type_Declarations;
with Gela.Elements.Qualified_Expressions;
with Gela.Elements.Quantified_Expressions;
with Gela.Elements.Raise_Statements;
with Gela.Elements.Range_Attribute_Reference_Drs;
with Gela.Elements.Range_Attribute_References;
with Gela.Elements.Record_Aggregates;
with Gela.Elements.Record_Definitions;
with Gela.Elements.Record_Representation_Clauses;
with Gela.Elements.Record_Type_Definitions;
with Gela.Elements.Requeue_Statements;
with Gela.Elements.Return_Object_Specifications;
with Gela.Elements.Scalar_Subtype_Indications;
with Gela.Elements.Select_Or_Paths;
with Gela.Elements.Selected_Components;
with Gela.Elements.Selected_Identifiers;
with Gela.Elements.Selective_Accepts;
with Gela.Elements.Short_Circuits;
with Gela.Elements.Signed_Integer_Type_Definitions;
with Gela.Elements.Simple_Expression_Range_Drs;
with Gela.Elements.Simple_Expression_Ranges;
with Gela.Elements.Simple_Return_Statements;
with Gela.Elements.Single_Protected_Declarations;
with Gela.Elements.Single_Task_Declarations;
with Gela.Elements.String_Literals;
with Gela.Elements.Subtype_Declarations;
with Gela.Elements.Subunits;
with Gela.Elements.Task_Bodies;
with Gela.Elements.Task_Body_Stubs;
with Gela.Elements.Task_Definitions;
with Gela.Elements.Task_Type_Declarations;
with Gela.Elements.Terminate_Alternative_Statements;
with Gela.Elements.Then_Abort_Paths;
with Gela.Elements.Unconstrained_Array_Definitions;
with Gela.Elements.Unknown_Discriminant_Parts;
with Gela.Elements.Use_Package_Clauses;
with Gela.Elements.Use_Type_Clauses;
with Gela.Elements.Variant_Parts;
with Gela.Elements.Variants;
with Gela.Elements.While_Loop_Statements;
with Gela.Elements.With_Clauses;
with Gela.Interpretations;
with Gela.Semantic_Types;
with Gela.Type_Managers;
with Gela.Types;

package body Asis.Extensions.Flat_Kinds is

   type Visiter is new Gela.Element_Visiters.Visiter with record
      Result : Element_Flat_Kind;
   end record;

   overriding procedure Compilation
     (Self : in out Visiter;
      Node : not null Gela.Elements.Compilations.Compilation_Access)
   is null;

   overriding procedure Abort_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Abort_Statements.Abort_Statement_Access)
   is null;

   overriding procedure Accept_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Accept_Statements.Accept_Statement_Access);

   overriding procedure Access_To_Function_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Access_To_Function_Definitions.
        Access_To_Function_Definition_Access)
   is null;

   overriding procedure Access_To_Object_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Access_To_Object_Definitions.
        Access_To_Object_Definition_Access);

   overriding procedure Access_To_Procedure_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Access_To_Procedure_Definitions.
        Access_To_Procedure_Definition_Access)
   is null;

   overriding procedure Allocator
     (Self : in out Visiter;
      Node : not null Gela.Elements.Allocators.Allocator_Access);

   overriding procedure Anonymous_Access_To_Function_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Anonymous_Access_To_Function_Definitions.
        Anonymous_Access_To_Function_Definition_Access)
   is null;

   overriding procedure Anonymous_Access_To_Object_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Anonymous_Access_To_Object_Definitions.
        Anonymous_Access_To_Object_Definition_Access);

   overriding procedure Anonymous_Access_To_Procedure_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Anonymous_Access_To_Procedure_Definitions.
        Anonymous_Access_To_Procedure_Definition_Access)
   is null;

   overriding procedure Aspect_Specification
     (Self : in out Visiter;
      Node : not null Gela.Elements.Aspect_Specifications.
        Aspect_Specification_Access)
   is null;

   overriding procedure Assignment_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Assignment_Statements.
        Assignment_Statement_Access);

   overriding procedure Association
     (Self : in out Visiter;
      Node : not null Gela.Elements.Associations.Association_Access);

   overriding procedure Asynchronous_Select
     (Self : in out Visiter;
      Node : not null Gela.Elements.Asynchronous_Selects.
        Asynchronous_Select_Access)
   is null;

   overriding procedure At_Clause
     (Self : in out Visiter;
      Node : not null Gela.Elements.At_Clauses.At_Clause_Access)
   is null;

   overriding procedure Attribute_Definition_Clause
     (Self : in out Visiter;
      Node : not null Gela.Elements.Attribute_Definition_Clauses.
        Attribute_Definition_Clause_Access)
   is null;

   overriding procedure Attribute_Reference
     (Self : in out Visiter;
      Node : not null Gela.Elements.Attribute_References.
        Attribute_Reference_Access);

   overriding procedure Function_Call
     (Self : in out Visiter;
      Node : not null Gela.Elements.Function_Calls.Function_Call_Access);

   overriding procedure Block_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Block_Statements.Block_Statement_Access);

   overriding procedure Box
     (Self : in out Visiter;
      Node : not null Gela.Elements.Boxes.Box_Access)
   is null;

   overriding procedure Case_Expression
     (Self : in out Visiter;
      Node : not null Gela.Elements.Case_Expressions.Case_Expression_Access)
   is null;

   overriding procedure Case_Expression_Path
     (Self : in out Visiter;
      Node : not null Gela.Elements.Case_Expression_Paths.
        Case_Expression_Path_Access)
   is null;

   overriding procedure Case_Path
     (Self : in out Visiter;
      Node : not null Gela.Elements.Case_Paths.Case_Path_Access);

   overriding procedure Case_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Case_Statements.Case_Statement_Access);

   overriding procedure Character_Literal
     (Self : in out Visiter;
      Node : not null Gela.Elements.Character_Literals.
        Character_Literal_Access);

   overriding procedure Choice_Parameter_Specification
     (Self : in out Visiter;
      Node : not null Gela.Elements.Choice_Parameter_Specifications.
        Choice_Parameter_Specification_Access)
   is null;

   overriding procedure Compilation_Unit_Body
     (Self : in out Visiter;
      Node : not null Gela.Elements.Compilation_Unit_Bodies.
        Compilation_Unit_Body_Access)
   is null;

   overriding procedure Compilation_Unit_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access)
   is null;

   overriding procedure Component_Clause
     (Self : in out Visiter;
      Node : not null Gela.Elements.Component_Clauses.Component_Clause_Access);

   overriding procedure Component_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Component_Declarations.
        Component_Declaration_Access);

   overriding procedure Component_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Component_Definitions.
        Component_Definition_Access);

   overriding procedure Composite_Constraint
     (Self : in out Visiter;
      Node : not null Gela.Elements.Composite_Constraints.
        Composite_Constraint_Access);

   overriding procedure Composite_Subtype_Indication
     (Self : in out Visiter;
      Node : not null Gela.Elements.Composite_Subtype_Indications.
        Composite_Subtype_Indication_Access);

   overriding procedure Constrained_Array_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Constrained_Array_Definitions.
        Constrained_Array_Definition_Access);

   overriding procedure Decimal_Fixed_Point_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Decimal_Fixed_Point_Definitions.
        Decimal_Fixed_Point_Definition_Access)
   is null;

   overriding procedure Defining_Character_Literal
     (Self : in out Visiter;
      Node : not null Gela.Elements.Defining_Character_Literals.
        Defining_Character_Literal_Access);

   overriding procedure Defining_Enumeration_Literal
     (Self : in out Visiter;
      Node : not null Gela.Elements.Defining_Enumeration_Literals.
        Defining_Enumeration_Literal_Access);

   overriding procedure Defining_Expanded_Unit_Name
     (Self : in out Visiter;
      Node : not null Gela.Elements.Defining_Expanded_Unit_Names.
        Defining_Expanded_Unit_Name_Access);

   overriding procedure Defining_Identifier
     (Self : in out Visiter;
      Node : not null Gela.Elements.Defining_Identifiers.
        Defining_Identifier_Access);

   overriding procedure Defining_Operator_Symbol
     (Self : in out Visiter;
      Node : not null Gela.Elements.Defining_Operator_Symbols.
        Defining_Operator_Symbol_Access);

   overriding procedure Delay_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Delay_Statements.Delay_Statement_Access);

   overriding procedure Delta_Constraint
     (Self : in out Visiter;
      Node : not null Gela.Elements.Delta_Constraints.Delta_Constraint_Access)
   is null;

   overriding procedure Derived_Record_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Derived_Record_Definitions.
        Derived_Record_Definition_Access)
   is null;

   overriding procedure Derived_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Derived_Type_Definitions.
        Derived_Type_Definition_Access);

   overriding procedure Digits_Constraint
     (Self : in out Visiter;
      Node : not null Gela.Elements.Digits_Constraints.
        Digits_Constraint_Access)
   is null;

   overriding procedure Discrete_Range_Attribute_Reference
     (Self : in out Visiter;
      Node : not null Gela.Elements.Discrete_Range_Attribute_References.
        Discrete_Range_Attribute_Reference_Access)
   is null;

   overriding procedure Discrete_Simple_Expression_Range
     (Self : in out Visiter;
      Node : not null Gela.Elements.Discrete_Simple_Expression_Ranges.
        Discrete_Simple_Expression_Range_Access);

   overriding procedure Discrete_Subtype_Indication
     (Self : in out Visiter;
      Node : not null Gela.Elements.Discrete_Subtype_Indications.
        Discrete_Subtype_Indication_Access);

   overriding procedure Discrete_Subtype_Indication_Dr
     (Self : in out Visiter;
      Node : not null Gela.Elements.Discrete_Subtype_Indication_Drs.
        Discrete_Subtype_Indication_Dr_Access)
   is null;

   overriding procedure Discriminant_Specification
     (Self : in out Visiter;
      Node : not null Gela.Elements.Discriminant_Specifications.
        Discriminant_Specification_Access);

   overriding procedure Element_Iterator_Specification
     (Self : in out Visiter;
      Node : not null Gela.Elements.Element_Iterator_Specifications.
        Element_Iterator_Specification_Access)
   is null;

   overriding procedure Else_Expression_Path
     (Self : in out Visiter;
      Node : not null Gela.Elements.Else_Expression_Paths.
        Else_Expression_Path_Access)
   is null;

   overriding procedure Else_Path
     (Self : in out Visiter;
      Node : not null Gela.Elements.Else_Paths.Else_Path_Access);

   overriding procedure Elsif_Expression_Path
     (Self : in out Visiter;
      Node : not null Gela.Elements.Elsif_Expression_Paths.
        Elsif_Expression_Path_Access)
   is null;

   overriding procedure Elsif_Path
     (Self : in out Visiter;
      Node : not null Gela.Elements.Elsif_Paths.Elsif_Path_Access)
   is null;

   overriding procedure Entry_Body
     (Self : in out Visiter;
      Node : not null Gela.Elements.Entry_Bodies.Entry_Body_Access);

   overriding procedure Entry_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Entry_Declarations.
        Entry_Declaration_Access);

   overriding procedure Entry_Index_Specification
     (Self : in out Visiter;
      Node : not null Gela.Elements.Entry_Index_Specifications.
        Entry_Index_Specification_Access)
   is null;

   overriding procedure Enumeration_Literal_Specification
     (Self : in out Visiter;
      Node : not null Gela.Elements.Enumeration_Literal_Specifications.
        Enumeration_Literal_Specification_Access);

   overriding procedure Enumeration_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Enumeration_Type_Definitions.
        Enumeration_Type_Definition_Access);

   overriding procedure Exception_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Exception_Declarations.
        Exception_Declaration_Access);

   overriding procedure Exception_Handler
     (Self : in out Visiter;
      Node : not null Gela.Elements.Exception_Handlers.
        Exception_Handler_Access);

   overriding procedure Exception_Renaming_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Exception_Renaming_Declarations.
        Exception_Renaming_Declaration_Access);

   overriding procedure Exit_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Exit_Statements.Exit_Statement_Access);

   overriding procedure Explicit_Dereference
     (Self : in out Visiter;
      Node : not null Gela.Elements.Explicit_Dereferences.
        Explicit_Dereference_Access);

   overriding procedure Extended_Return_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Extended_Return_Statements.
        Extended_Return_Statement_Access)
   is null;

   overriding procedure Extension_Aggregate
     (Self : in out Visiter;
      Node : not null Gela.Elements.Extension_Aggregates.
        Extension_Aggregate_Access)
   is null;

   overriding procedure Floating_Point_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Floating_Point_Definitions.
        Floating_Point_Definition_Access);

   overriding procedure For_Loop_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.For_Loop_Statements.
        For_Loop_Statement_Access);

   overriding procedure Formal_Access_To_Function_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Access_To_Function_Definitions.
        Formal_Access_To_Function_Definition_Access)
   is null;

   overriding procedure Formal_Access_To_Object_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Access_To_Object_Definitions.
        Formal_Access_To_Object_Definition_Access)
   is null;

   overriding procedure Formal_Access_To_Procedure_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Access_To_Procedure_Definitions.
        Formal_Access_To_Procedure_Definition_Access)
   is null;

   overriding procedure Formal_Constrained_Array_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Constrained_Array_Definitions.
        Formal_Constrained_Array_Definition_Access)
   is null;

   overriding procedure Formal_Decimal_Fixed_Point_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Decimal_Fixed_Point_Definitions.
        Formal_Decimal_Fixed_Point_Definition_Access)
   is null;

   overriding procedure Formal_Derived_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Derived_Type_Definitions.
        Formal_Derived_Type_Definition_Access);

   overriding procedure Formal_Discrete_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Discrete_Type_Definitions.
        Formal_Discrete_Type_Definition_Access);

   overriding procedure Formal_Floating_Point_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Floating_Point_Definitions.
        Formal_Floating_Point_Definition_Access)
   is null;

   overriding procedure Formal_Function_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Function_Declarations.
        Formal_Function_Declaration_Access)
   is null;

   overriding procedure Formal_Incomplete_Type_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Incomplete_Type_Declarations.
        Formal_Incomplete_Type_Declaration_Access)
   is null;

   overriding procedure Formal_Interface_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Interface_Type_Definitions.
        Formal_Interface_Type_Definition_Access)
   is null;

   overriding procedure Formal_Modular_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Modular_Type_Definitions.
        Formal_Modular_Type_Definition_Access)
   is null;

   overriding procedure Formal_Object_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Object_Declarations.
        Formal_Object_Declaration_Access);

   overriding procedure Formal_Ordinary_Fixed_Point_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Ordinary_Fixed_Point_Definitions.
        Formal_Ordinary_Fixed_Point_Definition_Access)
   is null;

   overriding procedure Formal_Package_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Package_Declarations.
        Formal_Package_Declaration_Access)
   is null;

   overriding procedure Formal_Private_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Private_Type_Definitions.
        Formal_Private_Type_Definition_Access)
   is null;

   overriding procedure Formal_Procedure_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Procedure_Declarations.
        Formal_Procedure_Declaration_Access)
   is null;

   overriding procedure Formal_Signed_Integer_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Signed_Integer_Type_Definitions.
        Formal_Signed_Integer_Type_Definition_Access);

   overriding procedure Formal_Type_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Type_Declarations.
        Formal_Type_Declaration_Access);

   overriding procedure Formal_Unconstrained_Array_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Unconstrained_Array_Definitions.
        Formal_Unconstrained_Array_Definition_Access)
   is null;

   overriding procedure Full_Type_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Full_Type_Declarations.
        Full_Type_Declaration_Access);

   overriding procedure Function_Body
     (Self : in out Visiter;
      Node : not null Gela.Elements.Function_Bodies.Function_Body_Access);

   overriding procedure Function_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Function_Declarations.
        Function_Declaration_Access);

   overriding procedure Function_Instantiation
     (Self : in out Visiter;
      Node : not null Gela.Elements.Function_Instantiations.
        Function_Instantiation_Access)
   is null;

   overriding procedure Generalized_Iterator_Specification
     (Self : in out Visiter;
      Node : not null Gela.Elements.Generalized_Iterator_Specifications.
        Generalized_Iterator_Specification_Access)
   is null;

   overriding procedure Generic_Association
     (Self : in out Visiter;
      Node : not null Gela.Elements.Generic_Associations.
        Generic_Association_Access);

   overriding procedure Generic_Function_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Generic_Function_Declarations.
        Generic_Function_Declaration_Access)
   is null;

   overriding procedure Generic_Function_Renaming
     (Self : in out Visiter;
      Node : not null Gela.Elements.Generic_Function_Renamings.
        Generic_Function_Renaming_Access)
   is null;

   overriding procedure Generic_Package_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Generic_Package_Declarations.
        Generic_Package_Declaration_Access);

   overriding procedure Generic_Package_Renaming
     (Self : in out Visiter;
      Node : not null Gela.Elements.Generic_Package_Renamings.
        Generic_Package_Renaming_Access)
   is null;

   overriding procedure Generic_Procedure_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Generic_Procedure_Declarations.
        Generic_Procedure_Declaration_Access)
   is null;

   overriding procedure Generic_Procedure_Renaming
     (Self : in out Visiter;
      Node : not null Gela.Elements.Generic_Procedure_Renamings.
        Generic_Procedure_Renaming_Access)
   is null;

   overriding procedure Goto_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Goto_Statements.Goto_Statement_Access)
   is null;

   overriding procedure Identifier
     (Self : in out Visiter;
      Node : not null Gela.Elements.Identifiers.Identifier_Access);

   overriding procedure If_Expression
     (Self : in out Visiter;
      Node : not null Gela.Elements.If_Expressions.If_Expression_Access)
   is null;

   overriding procedure If_Expression_Path
     (Self : in out Visiter;
      Node : not null Gela.Elements.If_Expression_Paths.
        If_Expression_Path_Access)
   is null;

   overriding procedure If_Path
     (Self : in out Visiter;
      Node : not null Gela.Elements.If_Paths.If_Path_Access);

   overriding procedure If_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.If_Statements.If_Statement_Access);

   overriding procedure Incomplete_Type_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Incomplete_Type_Declarations.
        Incomplete_Type_Declaration_Access);

   overriding procedure Incomplete_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Incomplete_Type_Definitions.
        Incomplete_Type_Definition_Access);

   overriding procedure Interface_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Interface_Type_Definitions.
        Interface_Type_Definition_Access)
   is null;

   overriding procedure Known_Discriminant_Part
     (Self : in out Visiter;
      Node : not null Gela.Elements.Known_Discriminant_Parts.
        Known_Discriminant_Part_Access);

   overriding procedure Label_Decorator
     (Self : in out Visiter;
      Node : not null Gela.Elements.Label_Decorators.Label_Decorator_Access)
   is null;

   overriding procedure Loop_Parameter_Specification
     (Self : in out Visiter;
      Node : not null Gela.Elements.Loop_Parameter_Specifications.
        Loop_Parameter_Specification_Access);

   overriding procedure Loop_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Loop_Statements.Loop_Statement_Access);

   overriding procedure Membership_Test
     (Self : in out Visiter;
      Node : not null Gela.Elements.Membership_Tests.Membership_Test_Access);

   overriding procedure Modular_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Modular_Type_Definitions.
        Modular_Type_Definition_Access)
   is null;

   overriding procedure Null_Component
     (Self : in out Visiter;
      Node : not null Gela.Elements.Null_Components.Null_Component_Access);

   overriding procedure Null_Literal
     (Self : in out Visiter;
      Node : not null Gela.Elements.Null_Literals.Null_Literal_Access)
   is null;

   overriding procedure Null_Record_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Null_Record_Definitions.
        Null_Record_Definition_Access)
   is null;

   overriding procedure Null_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Null_Statements.Null_Statement_Access);

   overriding procedure Number_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Number_Declarations.
        Number_Declaration_Access);

   overriding procedure Numeric_Literal
     (Self : in out Visiter;
      Node : not null Gela.Elements.Numeric_Literals.Numeric_Literal_Access);

   overriding procedure Object_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Object_Declarations.
        Object_Declaration_Access);

   overriding procedure Object_Renaming_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Object_Renaming_Declarations.
        Object_Renaming_Declaration_Access);

   overriding procedure Operator_Symbol
     (Self : in out Visiter;
      Node : not null Gela.Elements.Operator_Symbols.Operator_Symbol_Access);

   overriding procedure Ordinary_Fixed_Point_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Ordinary_Fixed_Point_Definitions.
        Ordinary_Fixed_Point_Definition_Access);

   overriding procedure Others_Choice
     (Self : in out Visiter;
      Node : not null Gela.Elements.Others_Choices.Others_Choice_Access);

   overriding procedure Package_Body
     (Self : in out Visiter;
      Node : not null Gela.Elements.Package_Bodies.Package_Body_Access);

   overriding procedure Package_Body_Stub
     (Self : in out Visiter;
      Node : not null Gela.Elements.Package_Body_Stubs.
        Package_Body_Stub_Access);

   overriding procedure Package_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Package_Declarations.
        Package_Declaration_Access);

   overriding procedure Package_Instantiation
     (Self : in out Visiter;
      Node : not null Gela.Elements.Package_Instantiations.
        Package_Instantiation_Access);

   overriding procedure Package_Renaming_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Package_Renaming_Declarations.
        Package_Renaming_Declaration_Access);

   overriding procedure Parameter_Association
     (Self : in out Visiter;
      Node : not null Gela.Elements.Parameter_Associations.
        Parameter_Association_Access)
   is null;

   overriding procedure Parameter_Specification
     (Self : in out Visiter;
      Node : not null Gela.Elements.Parameter_Specifications.
        Parameter_Specification_Access);

   overriding procedure Parenthesized_Expression
     (Self : in out Visiter;
      Node : not null Gela.Elements.Parenthesized_Expressions.
        Parenthesized_Expression_Access);

   overriding procedure Pragma_Argument_Association
     (Self : in out Visiter;
      Node : not null Gela.Elements.Pragma_Argument_Associations.
        Pragma_Argument_Association_Access)
   is null;

   overriding procedure Pragma_Node
     (Self : in out Visiter;
      Node : not null Gela.Elements.Pragma_Nodes.Pragma_Node_Access);

   overriding procedure Private_Extension_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Private_Extension_Declarations.
        Private_Extension_Declaration_Access)
   is null;

   overriding procedure Private_Extension_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Private_Extension_Definitions.
        Private_Extension_Definition_Access)
   is null;

   overriding procedure Private_Type_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Private_Type_Declarations.
        Private_Type_Declaration_Access);

   overriding procedure Private_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Private_Type_Definitions.
        Private_Type_Definition_Access);

   overriding procedure Procedure_Body
     (Self : in out Visiter;
      Node : not null Gela.Elements.Procedure_Bodies.Procedure_Body_Access);

   overriding procedure Procedure_Call_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Procedure_Call_Statements.
        Procedure_Call_Statement_Access);

   overriding procedure Procedure_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Procedure_Declarations.
        Procedure_Declaration_Access);

   overriding procedure Procedure_Instantiation
     (Self : in out Visiter;
      Node : not null Gela.Elements.Procedure_Instantiations.
        Procedure_Instantiation_Access)
   is null;

   overriding procedure Protected_Body
     (Self : in out Visiter;
      Node : not null Gela.Elements.Protected_Bodies.Protected_Body_Access);

   overriding procedure Protected_Body_Stub
     (Self : in out Visiter;
      Node : not null Gela.Elements.Protected_Body_Stubs.
        Protected_Body_Stub_Access)
   is null;

   overriding procedure Protected_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Protected_Definitions.
        Protected_Definition_Access);

   overriding procedure Protected_Type_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Protected_Type_Declarations.
        Protected_Type_Declaration_Access)
   is null;

   overriding procedure Qualified_Expression
     (Self : in out Visiter;
      Node : not null Gela.Elements.Qualified_Expressions.
        Qualified_Expression_Access);

   overriding procedure Quantified_Expression
     (Self : in out Visiter;
      Node : not null Gela.Elements.Quantified_Expressions.
        Quantified_Expression_Access)
   is null;

   overriding procedure Raise_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Raise_Statements.Raise_Statement_Access)
   is null;

   overriding procedure Range_Attribute_Reference
     (Self : in out Visiter;
      Node : not null Gela.Elements.Range_Attribute_References.
        Range_Attribute_Reference_Access);

   overriding procedure Range_Attribute_Reference_Dr
     (Self : in out Visiter;
      Node : not null Gela.Elements.Range_Attribute_Reference_Drs.
        Range_Attribute_Reference_Dr_Access)
   is null;

   overriding procedure Association_List
     (Self : in out Visiter;
      Node : not null Gela.Elements.Association_Lists.Association_List_Access)
        is null;

   overriding procedure Record_Aggregate
     (Self : in out Visiter;
      Node : not null Gela.Elements.Record_Aggregates.Record_Aggregate_Access);

   overriding procedure Record_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Record_Definitions.
        Record_Definition_Access);

   overriding procedure Record_Representation_Clause
     (Self : in out Visiter;
      Node : not null Gela.Elements.Record_Representation_Clauses.
        Record_Representation_Clause_Access);

   overriding procedure Record_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Record_Type_Definitions.
        Record_Type_Definition_Access);

   overriding procedure Requeue_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Requeue_Statements.
        Requeue_Statement_Access);

   overriding procedure Return_Object_Specification
     (Self : in out Visiter;
      Node : not null Gela.Elements.Return_Object_Specifications.
        Return_Object_Specification_Access)
   is null;

   overriding procedure Scalar_Subtype_Indication
     (Self : in out Visiter;
      Node : not null Gela.Elements.Scalar_Subtype_Indications.
        Scalar_Subtype_Indication_Access);

   overriding procedure Select_Or_Path
     (Self : in out Visiter;
      Node : not null Gela.Elements.Select_Or_Paths.Select_Or_Path_Access);

   overriding procedure Selected_Component
     (Self : in out Visiter;
      Node : not null Gela.Elements.Selected_Components.
        Selected_Component_Access);

   overriding procedure Selected_Identifier
     (Self : in out Visiter;
      Node : not null Gela.Elements.Selected_Identifiers.
        Selected_Identifier_Access);

   overriding procedure Selective_Accept
     (Self : in out Visiter;
      Node : not null Gela.Elements.Selective_Accepts.Selective_Accept_Access);

   overriding procedure Short_Circuit
     (Self : in out Visiter;
      Node : not null Gela.Elements.Short_Circuits.Short_Circuit_Access)
   is null;

   overriding procedure Signed_Integer_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Signed_Integer_Type_Definitions.
        Signed_Integer_Type_Definition_Access);

   overriding procedure Simple_Expression_Range
     (Self : in out Visiter;
      Node : not null Gela.Elements.Simple_Expression_Ranges.
        Simple_Expression_Range_Access);

   overriding procedure Simple_Expression_Range_Dr
     (Self : in out Visiter;
      Node : not null Gela.Elements.Simple_Expression_Range_Drs.
        Simple_Expression_Range_Dr_Access);

   overriding procedure Simple_Return_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Simple_Return_Statements.
        Simple_Return_Statement_Access);

   overriding procedure Single_Protected_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Single_Protected_Declarations.
        Single_Protected_Declaration_Access);

   overriding procedure Single_Task_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Single_Task_Declarations.
        Single_Task_Declaration_Access);

   overriding procedure String_Literal
     (Self : in out Visiter;
      Node : not null Gela.Elements.String_Literals.String_Literal_Access);

   overriding procedure Subtype_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Subtype_Declarations.
        Subtype_Declaration_Access);

   overriding procedure Subunit
     (Self : in out Visiter;
      Node : not null Gela.Elements.Subunits.Subunit_Access)
   is null;

   overriding procedure Task_Body
     (Self : in out Visiter;
      Node : not null Gela.Elements.Task_Bodies.Task_Body_Access);

   overriding procedure Task_Body_Stub
     (Self : in out Visiter;
      Node : not null Gela.Elements.Task_Body_Stubs.Task_Body_Stub_Access);

   overriding procedure Task_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Task_Definitions.Task_Definition_Access);

   overriding procedure Task_Type_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Task_Type_Declarations.
        Task_Type_Declaration_Access)
   is null;

   overriding procedure Terminate_Alternative_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Terminate_Alternative_Statements.
        Terminate_Alternative_Statement_Access);

   overriding procedure Then_Abort_Path
     (Self : in out Visiter;
      Node : not null Gela.Elements.Then_Abort_Paths.Then_Abort_Path_Access)
   is null;

   overriding procedure Unconstrained_Array_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Unconstrained_Array_Definitions.
        Unconstrained_Array_Definition_Access);

   overriding procedure Unknown_Discriminant_Part
     (Self : in out Visiter;
      Node : not null Gela.Elements.Unknown_Discriminant_Parts.
        Unknown_Discriminant_Part_Access)
   is null;

   overriding procedure Use_Package_Clause
     (Self : in out Visiter;
      Node : not null Gela.Elements.Use_Package_Clauses.
        Use_Package_Clause_Access);

   overriding procedure Use_Type_Clause
     (Self : in out Visiter;
      Node : not null Gela.Elements.Use_Type_Clauses.Use_Type_Clause_Access);

   overriding procedure Variant
     (Self : in out Visiter;
      Node : not null Gela.Elements.Variants.Variant_Access);

   overriding procedure Variant_Part
     (Self : in out Visiter;
      Node : not null Gela.Elements.Variant_Parts.Variant_Part_Access);

   overriding procedure While_Loop_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.While_Loop_Statements.
        While_Loop_Statement_Access)
   is null;

   overriding procedure With_Clause
     (Self : in out Visiter;
      Node : not null Gela.Elements.With_Clauses.With_Clause_Access);

   overriding procedure Accept_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Accept_Statements.Accept_Statement_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Accept_Statement;
   end Accept_Statement;

   overriding procedure Access_To_Object_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Access_To_Object_Definitions.
        Access_To_Object_Definition_Access)
   is
      use type Gela.Lexical_Types.Token_Count;
      use type Gela.Lexical_Types.Token_Kind;

      Comp  : constant Gela.Compilations.Compilation_Access :=
        Node.Enclosing_Compilation;
      Token : Gela.Lexical_Types.Token;
   begin
      if Node.Constant_Token = 0 then
         Self.Result := A_Pool_Specific_Access_To_Variable;
         return;
      end if;

      Token := Comp.Get_Token (Node.Constant_Token);

      if Token.Kind = Gela.Lexical_Types.All_Token then
         Self.Result := An_Access_To_Variable;
      else
         Self.Result := An_Access_To_Constant;
      end if;
   end Access_To_Object_Definition;

   overriding procedure Allocator
     (Self : in out Visiter;
      Node : not null Gela.Elements.Allocators.Allocator_Access)
   is
      Name : constant Gela.Elements.Names.Name_Access :=
        Node.Subtype_Or_Expression;
   begin
      Name.Visit (Self);

      case Self.Result is
         when A_Qualified_Expression =>
            Self.Result := An_Allocation_From_Qualified_Expression;
         when others =>
            Self.Result := An_Allocation_From_Subtype;
      end case;
   end Allocator;

   overriding procedure Anonymous_Access_To_Object_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Anonymous_Access_To_Object_Definitions.
        Anonymous_Access_To_Object_Definition_Access)
   is
      use type Gela.Lexical_Types.Token_Count;
   begin
      if Node.Constant_Token = 0 then
         Self.Result := An_Anonymous_Access_To_Variable;
      else
         Self.Result := An_Anonymous_Access_To_Constant;
      end if;
   end Anonymous_Access_To_Object_Definition;

   overriding procedure Assignment_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Assignment_Statements.
        Assignment_Statement_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Assignment_Statement;
   end Assignment_Statement;

   overriding procedure Association
     (Self : in out Visiter;
      Node : not null Gela.Elements.Associations.Association_Access)
   is
      Parent : Gela.Elements.Element_Access := Node.Enclosing_Element;
   begin
      while Auxilary ((Data => Parent)) loop
         Parent := Parent.Enclosing_Element;
      end loop;

      Parent.Visit (Self);

      case Self.Result is
         when A_Record_Aggregate =>
            Self.Result := A_Record_Component_Association;
         when A_Positional_Array_Aggregate | A_Named_Array_Aggregate =>
            Self.Result := An_Array_Component_Association;
         when A_Discriminant_Constraint =>
            Self.Result := A_Discriminant_Association;
         when others =>
            Self.Result := A_Parameter_Association;
      end case;
   end Association;

   overriding procedure Attribute_Reference
     (Self : in out Visiter;
      Node : not null Gela.Elements.Attribute_References.
        Attribute_Reference_Access)
   is
      package X renames Gela.Lexical_Types.Predefined_Symbols;

      Id    : constant Gela.Elements.Identifiers.Identifier_Access :=
        Node.Attribute_Designator_Identifier;
      Comp  : constant Gela.Compilations.Compilation_Access :=
        Node.Enclosing_Compilation;
      Token : Gela.Lexical_Types.Token;
      Map   : constant array (Gela.Lexical_Types.Symbol range
                              X.Access_Symbol .. X.Write) of Element_Flat_Kind
        :=
          (
           X.Access_Symbol => An_Access_Attribute,
           X.Address => An_Address_Attribute,
           X.Adjacent => An_Adjacent_Attribute,
           X.Aft => An_Aft_Attribute,
           X.Alignment => An_Alignment_Attribute,
           X.Base => A_Base_Attribute,
           X.Bit_Order => A_Bit_Order_Attribute,
           X.Body_Version => A_Body_Version_Attribute,
           X.Callable => A_Callable_Attribute,
           X.Caller => A_Caller_Attribute,
           X.Ceiling => A_Ceiling_Attribute,
           X.Class => A_Class_Attribute,
           X.Component_Size => A_Component_Size_Attribute,
           X.Compose => A_Compose_Attribute,
           X.Constrained => A_Constrained_Attribute,
           X.Copy_Sign => A_Copy_Sign_Attribute,
           X.Count => A_Count_Attribute,
           X.Definite => A_Definite_Attribute,
           X.Delta_Symbol => A_Delta_Attribute,
           X.Denorm => A_Denorm_Attribute,
           X.Digits_Symbol => A_Digits_Attribute,
           X.Exponent => An_Exponent_Attribute,
           X.External_Tag => An_External_Tag_Attribute,
           X.First => A_First_Attribute,
           X.First_Bit => A_First_Bit_Attribute,
           X.Floor => A_Floor_Attribute,
           X.Fore => A_Fore_Attribute,
           X.Fraction => A_Fraction_Attribute,
           X.Identity => An_Identity_Attribute,
           X.Image => An_Image_Attribute,
           X.Input => An_Input_Attribute,
           X.Last => A_Last_Attribute,
           X.Last_Bit => A_Last_Bit_Attribute,
           X.Leading_Part => A_Leading_Part_Attribute,
           X.Length => A_Length_Attribute,
           X.Machine => A_Machine_Attribute,
           X.Machine_Emax => A_Machine_Emax_Attribute,
           X.Machine_Emin => A_Machine_Emin_Attribute,
           X.Machine_Mantissa => A_Machine_Mantissa_Attribute,
           X.Machine_Overflows => A_Machine_Overflows_Attribute,
           X.Machine_Radix => A_Machine_Radix_Attribute,
           X.Machine_Rounding => A_Machine_Rounding_Attribute,
           X.Machine_Rounds => A_Machine_Rounds_Attribute,
           X.Max => A_Max_Attribute,
           X.Max_Size_In_Storage_Elements =>
             A_Max_Size_In_Storage_Elements_Attribute,
           X.Min => A_Min_Attribute,
           X.Mod_Symbol => A_Mod_Attribute,
           X.Model => A_Model_Attribute,
           X.Model_Emin => A_Model_Emin_Attribute,
           X.Model_Epsilon => A_Model_Epsilon_Attribute,
           X.Model_Mantissa => A_Model_Mantissa_Attribute,
           X.Model_Small => A_Model_Small_Attribute,
           X.Modulus => A_Modulus_Attribute,
           X.Output => An_Output_Attribute,
           X.Partition_ID => A_Partition_ID_Attribute,
           X.Pos => A_Pos_Attribute,
           X.Position => A_Position_Attribute,
           X.Pred => A_Pred_Attribute,
           X.Priority => A_Priority_Attribute,
           X.Range_Symbol => A_Range_Attribute,
           X.Read => A_Read_Attribute,
           X.Remainder => A_Remainder_Attribute,
           X.Round => A_Round_Attribute,
           X.Rounding => A_Rounding_Attribute,
           X.Safe_First => A_Safe_First_Attribute,
           X.Safe_Last => A_Safe_Last_Attribute,
           X.Scale => A_Scale_Attribute,
           X.Scaling => A_Scaling_Attribute,
           X.Signed_Zeros => A_Signed_Zeros_Attribute,
           X.Size => A_Size_Attribute,
           X.Small => A_Small_Attribute,
           X.Storage_Pool => A_Storage_Pool_Attribute,
           X.Storage_Size => A_Storage_Size_Attribute,
           X.Stream_Size => A_Stream_Size_Attribute,
           X.Succ => A_Succ_Attribute,
           X.Tag => A_Tag_Attribute,
           X.Terminated => A_Terminated_Attribute,
           X.Truncation => A_Truncation_Attribute,
           X.Unbiased_Rounding => An_Unbiased_Rounding_Attribute,
           X.Unchecked_Access => An_Unchecked_Access_Attribute,
           X.Val => A_Val_Attribute,
           X.Valid => A_Valid_Attribute,
           X.Value => A_Value_Attribute,
           X.Version => A_Version_Attribute,
           X.Wide_Image => A_Wide_Image_Attribute,
           X.Wide_Value => A_Wide_Value_Attribute,
           X.Wide_Wide_Image => A_Wide_Wide_Image_Attribute,
           X.Wide_Wide_Value => A_Wide_Wide_Value_Attribute,
           X.Wide_Wide_Width => A_Wide_Wide_Width_Attribute,
           X.Wide_Width => A_Wide_Width_Attribute,
           X.Width => A_Width_Attribute,
           X.Write => A_Write_Attribute);
   begin
      Token := Comp.Get_Token (Id.Identifier_Token);

      if Token.Symbol in Map'Range then
         Self.Result := Map (Token.Symbol);
      else
         Self.Result := A_Range_Attribute;
      end if;
   end Attribute_Reference;

   overriding procedure Function_Call
     (Self : in out Visiter;
      Node : not null Gela.Elements.Function_Calls.Function_Call_Access)
   is
      use type Gela.Interpretations.Auxiliary_Apply_Kinds;
   begin
      case Node.Chosen_Interpretation is
         when Gela.Interpretations.Indexed_Component =>
            Self.Result := An_Indexed_Component;
         when Gela.Interpretations.Function_Call =>
            Self.Result := A_Function_Call;
         when Gela.Interpretations.Type_Convertion =>
            Self.Result := A_Type_Conversion;
         when others =>
            Self.Result := A_Function_Call;  --  ??? raise exception?
      end case;
   end Function_Call;

   overriding procedure Block_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Block_Statements.Block_Statement_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Block_Statement;
   end Block_Statement;

   overriding procedure Case_Path
     (Self : in out Visiter;
      Node : not null Gela.Elements.Case_Paths.Case_Path_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Case_Path;
   end Case_Path;

   overriding procedure Case_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Case_Statements.Case_Statement_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Case_Statement;
   end Case_Statement;

   overriding procedure Character_Literal
     (Self : in out Visiter;
      Node : not null Gela.Elements.Character_Literals.
        Character_Literal_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Character_Literal;
   end Character_Literal;

   overriding procedure Component_Clause
     (Self : in out Visiter;
      Node : not null Gela.Elements.Component_Clauses.Component_Clause_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Component_Clause;
   end Component_Clause;

   overriding procedure Component_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Component_Declarations.
        Component_Declaration_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Component_Declaration;
   end Component_Declaration;

   overriding procedure Component_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Component_Definitions.
        Component_Definition_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Component_Definition;
   end Component_Definition;

   overriding procedure Composite_Constraint
     (Self : in out Visiter;
      Node : not null Gela.Elements.Composite_Constraints.
        Composite_Constraint_Access)
   is
      use type Gela.Interpretations.Interpretation_Kinds;
   begin
      if Node.Chosen_Interpretation =
        Gela.Interpretations.Index_Constraint
      then
         Self.Result := An_Index_Constraint;
      else
         Self.Result := A_Discriminant_Constraint;
      end if;
   end Composite_Constraint;

   overriding procedure Composite_Subtype_Indication
     (Self : in out Visiter;
      Node : not null Gela.Elements.Composite_Subtype_Indications.
        Composite_Subtype_Indication_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Subtype_Indication;
   end Composite_Subtype_Indication;

   overriding procedure Constrained_Array_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Constrained_Array_Definitions.
        Constrained_Array_Definition_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Constrained_Array_Definition;
   end Constrained_Array_Definition;

   overriding procedure Defining_Character_Literal
     (Self : in out Visiter;
      Node : not null Gela.Elements.Defining_Character_Literals.
        Defining_Character_Literal_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Defining_Character_Literal;
   end Defining_Character_Literal;

   overriding procedure Defining_Enumeration_Literal
     (Self : in out Visiter;
      Node : not null Gela.Elements.Defining_Enumeration_Literals.
        Defining_Enumeration_Literal_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Defining_Enumeration_Literal;
   end Defining_Enumeration_Literal;

   overriding procedure Defining_Expanded_Unit_Name
     (Self : in out Visiter;
      Node : not null Gela.Elements.Defining_Expanded_Unit_Names.
        Defining_Expanded_Unit_Name_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Defining_Expanded_Name;
   end Defining_Expanded_Unit_Name;

   overriding procedure Defining_Identifier
     (Self : in out Visiter;
      Node : not null Gela.Elements.Defining_Identifiers.
        Defining_Identifier_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Defining_Identifier;
   end Defining_Identifier;

   overriding procedure Defining_Operator_Symbol
     (Self : in out Visiter;
      Node : not null Gela.Elements.Defining_Operator_Symbols.
        Defining_Operator_Symbol_Access)
   is
      Operator_Map : constant array
        (Gela.Lexical_Types.Symbol range 1 .. 19) of Element_Flat_Kind
        := (A_Defining_Less_Than_Operator,
            A_Defining_Equal_Operator,
            A_Defining_Greater_Than_Operator,
            A_Defining_Minus_Operator,
            A_Defining_Divide_Operator,
            A_Defining_Multiply_Operator,
            A_Defining_Concatenate_Operator,
            A_Defining_Plus_Operator,
            A_Defining_Less_Than_Or_Equal_Operator,
            A_Defining_Greater_Than_Or_Equal_Operator,
            A_Defining_Not_Equal_Operator,
            A_Defining_Exponentiate_Operator,
            A_Defining_Or_Operator,
            A_Defining_And_Operator,
            A_Defining_Xor_Operator,
            A_Defining_Mod_Operator,
            A_Defining_Rem_Operator,
            A_Defining_Abs_Operator,
            A_Defining_Not_Operator);

      Comp    : constant Gela.Compilations.Compilation_Access :=
        Node.Enclosing_Compilation;
      Token : constant Gela.Lexical_Types.Token :=
        Comp.Get_Token (Node.Operator_Symbol_Token);
   begin
      if Token.Symbol in Operator_Map'Range then
         Self.Result := Operator_Map (Token.Symbol);
      end if;
   end Defining_Operator_Symbol;

   overriding procedure Delay_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Delay_Statements.Delay_Statement_Access)
   is
      use type Gela.Lexical_Types.Token_Index;
   begin
      if Node.Until_Token = 0 then
         Self.Result := A_Delay_Relative_Statement;
      else
         Self.Result := A_Delay_Until_Statement;
      end if;
   end Delay_Statement;

   overriding procedure Derived_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Derived_Type_Definitions.
        Derived_Type_Definition_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Derived_Type_Definition;
   end Derived_Type_Definition;

   overriding procedure Discrete_Simple_Expression_Range
     (Self : in out Visiter;
      Node : not null Gela.Elements.Discrete_Simple_Expression_Ranges.
        Discrete_Simple_Expression_Range_Access) is
   begin
      Node.Enclosing_Element.Visit (Self);

      case Self.Result is
         when A_Component_Clause =>
            Self.Result := A_Discrete_Simple_Expression_Range_DR;
         when others =>
            Self.Result := A_Discrete_Simple_Expression_Range;
      end case;
   end Discrete_Simple_Expression_Range;

   overriding procedure Discrete_Subtype_Indication
     (Self : in out Visiter;
      Node : not null Gela.Elements.Discrete_Subtype_Indications.
        Discrete_Subtype_Indication_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Discrete_Subtype_Indication;
   end Discrete_Subtype_Indication;

   overriding procedure Discriminant_Specification
     (Self : in out Visiter;
      Node : not null Gela.Elements.Discriminant_Specifications.
        Discriminant_Specification_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Discriminant_Specification;
   end Discriminant_Specification;

   overriding procedure Else_Path
     (Self : in out Visiter;
      Node : not null Gela.Elements.Else_Paths.Else_Path_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Else_Path;
   end Else_Path;

   overriding procedure Entry_Body
     (Self : in out Visiter;
      Node : not null Gela.Elements.Entry_Bodies.Entry_Body_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Entry_Body_Declaration;
   end Entry_Body;

   overriding procedure Entry_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Entry_Declarations.
        Entry_Declaration_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Entry_Declaration;
   end Entry_Declaration;

   overriding procedure Enumeration_Literal_Specification
     (Self : in out Visiter;
      Node : not null Gela.Elements.Enumeration_Literal_Specifications.
        Enumeration_Literal_Specification_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Enumeration_Literal_Specification;
   end Enumeration_Literal_Specification;

   overriding procedure Enumeration_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Enumeration_Type_Definitions.
        Enumeration_Type_Definition_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Enumeration_Type_Definition;
   end Enumeration_Type_Definition;

   overriding procedure Exception_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Exception_Declarations.
        Exception_Declaration_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Exception_Declaration;
   end Exception_Declaration;

   overriding procedure Exception_Handler
     (Self : in out Visiter;
      Node : not null Gela.Elements.Exception_Handlers.
        Exception_Handler_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Exception_Handler;
   end Exception_Handler;

   overriding procedure Exception_Renaming_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Exception_Renaming_Declarations.
        Exception_Renaming_Declaration_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Exception_Renaming_Declaration;
   end Exception_Renaming_Declaration;

   overriding procedure Exit_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Exit_Statements.Exit_Statement_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Exit_Statement;
   end Exit_Statement;

   overriding procedure Explicit_Dereference
     (Self : in out Visiter;
      Node : not null Gela.Elements.Explicit_Dereferences.
        Explicit_Dereference_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Explicit_Dereference;
   end Explicit_Dereference;

   ---------------
   -- Flat_Kind --
   ---------------

   function Flat_Kind (Element : Asis.Element) return Element_Flat_Kind is
      V : aliased Visiter := (Result => Not_An_Element);
   begin
      Element.Data.Visit (V);
      pragma Assert
        (Element.Data.Assigned and then V.Result /= Not_An_Element);
      return V.Result;
   end Flat_Kind;

   overriding procedure Floating_Point_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Floating_Point_Definitions.
        Floating_Point_Definition_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Floating_Point_Definition;
   end Floating_Point_Definition;

   overriding procedure For_Loop_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.For_Loop_Statements.
        For_Loop_Statement_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_For_Loop_Statement;
   end For_Loop_Statement;

   overriding procedure Formal_Derived_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Derived_Type_Definitions.
        Formal_Derived_Type_Definition_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Formal_Derived_Type_Definition;
   end Formal_Derived_Type_Definition;

   overriding procedure Formal_Discrete_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Discrete_Type_Definitions.
        Formal_Discrete_Type_Definition_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Formal_Discrete_Type_Definition;
   end Formal_Discrete_Type_Definition;

   overriding procedure Formal_Object_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Object_Declarations.
        Formal_Object_Declaration_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Formal_Object_Declaration;
   end Formal_Object_Declaration;

   overriding procedure Formal_Signed_Integer_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Signed_Integer_Type_Definitions.
        Formal_Signed_Integer_Type_Definition_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Formal_Signed_Integer_Type_Definition;
   end Formal_Signed_Integer_Type_Definition;

   overriding procedure Formal_Type_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Formal_Type_Declarations.
        Formal_Type_Declaration_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Formal_Type_Declaration;
   end Formal_Type_Declaration;

   overriding procedure Full_Type_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Full_Type_Declarations.
        Full_Type_Declaration_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Ordinary_Type_Declaration;
   end Full_Type_Declaration;

   overriding procedure Function_Body
     (Self : in out Visiter;
      Node : not null Gela.Elements.Function_Bodies.Function_Body_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Function_Body_Declaration;
   end Function_Body;

   overriding procedure Function_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Function_Declarations.
        Function_Declaration_Access)
   is
      use type Gela.Lexical_Types.Token_Count;
   begin
      if Node.Renames_Token = 0 then
         Self.Result := A_Function_Declaration;
      else
         Self.Result := A_Function_Renaming_Declaration;
      end if;
   end Function_Declaration;

   overriding procedure Generic_Association
     (Self : in out Visiter;
      Node : not null Gela.Elements.Generic_Associations.
        Generic_Association_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Generic_Association;
   end Generic_Association;

   overriding procedure Generic_Package_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Generic_Package_Declarations.
        Generic_Package_Declaration_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Generic_Package_Declaration;
   end Generic_Package_Declaration;

   overriding procedure Identifier
     (Self : in out Visiter;
      Node : not null Gela.Elements.Identifiers.Identifier_Access)
   is
      Name : constant Gela.Elements.Defining_Names.Defining_Name_Access :=
        Node.Defining_Name;
   begin
      if Name.Assigned then
         Name.Visit (Self);

         if Self.Result = A_Defining_Enumeration_Literal then
            Self.Result := An_Enumeration_Literal;

            return;
         end if;
      end if;

      Self.Result := An_Identifier;
   end Identifier;

   overriding procedure If_Path
     (Self : in out Visiter;
      Node : not null Gela.Elements.If_Paths.If_Path_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_If_Path;
   end If_Path;

   overriding procedure If_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.If_Statements.If_Statement_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_If_Statement;
   end If_Statement;

   overriding procedure Incomplete_Type_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Incomplete_Type_Declarations.
        Incomplete_Type_Declaration_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Incomplete_Type_Declaration;
   end Incomplete_Type_Declaration;

   overriding procedure Incomplete_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Incomplete_Type_Definitions.
        Incomplete_Type_Definition_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Incomplete_Type_Definition;
   end Incomplete_Type_Definition;

   overriding procedure Known_Discriminant_Part
     (Self : in out Visiter;
      Node : not null Gela.Elements.Known_Discriminant_Parts.
        Known_Discriminant_Part_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Known_Discriminant_Part;
   end Known_Discriminant_Part;

   overriding procedure Loop_Parameter_Specification
     (Self : in out Visiter;
      Node : not null Gela.Elements.Loop_Parameter_Specifications.
        Loop_Parameter_Specification_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Loop_Parameter_Specification;
   end Loop_Parameter_Specification;

   overriding procedure Loop_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Loop_Statements.Loop_Statement_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Loop_Statement;
   end Loop_Statement;

   overriding procedure Membership_Test
     (Self : in out Visiter;
      Node : not null Gela.Elements.Membership_Tests.Membership_Test_Access)
   is
      use type Gela.Lexical_Types.Token_Count;
   begin
      if Node.Not_Token = 0 then
         Self.Result := An_In_Membership_Test;
      else
         Self.Result := A_Not_In_Membership_Test;
      end if;
   end Membership_Test;

   overriding procedure Null_Component
     (Self : in out Visiter;
      Node : not null Gela.Elements.Null_Components.Null_Component_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Null_Component;
   end Null_Component;

   overriding procedure Null_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Null_Statements.Null_Statement_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Null_Statement;
   end Null_Statement;

   overriding procedure Number_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Number_Declarations.
        Number_Declaration_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Integer_Number_Declaration;
   end Number_Declaration;

   overriding procedure Numeric_Literal
     (Self : in out Visiter;
      Node : not null Gela.Elements.Numeric_Literals.Numeric_Literal_Access)
   is
      Comp    : constant Gela.Compilations.Compilation_Access :=
        Node.Enclosing_Compilation;
      Source  : constant League.Strings.Universal_String := Comp.Source;
      Token   : constant Gela.Lexical_Types.Token :=
        Comp.Get_Token (Node.Numeric_Literal_Token);
      Point : constant Natural := Source.Index (Token.First, Token.Last, '.');
   begin
      if Point > 0 then
         Self.Result := A_Real_Literal;
      else
         Self.Result := An_Integer_Literal;
      end if;
   end Numeric_Literal;

   overriding procedure Object_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Object_Declarations.
        Object_Declaration_Access)
   is
      use type Gela.Lexical_Types.Token_Count;
   begin
      if Node.Constant_Token = 0 then
         Self.Result := A_Variable_Declaration;
      elsif Node.Initialization_Expression in null then
         Self.Result := A_Deferred_Constant_Declaration;
      else
         Self.Result := A_Constant_Declaration;
      end if;
   end Object_Declaration;

   overriding procedure Object_Renaming_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Object_Renaming_Declarations.
        Object_Renaming_Declaration_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Object_Renaming_Declaration;
   end Object_Renaming_Declaration;

   overriding procedure Operator_Symbol
     (Self : in out Visiter;
      Node : not null Gela.Elements.Operator_Symbols.Operator_Symbol_Access)
   is
      Operator_Map : constant array
        (Gela.Lexical_Types.Symbol range 1 .. 19) of Element_Flat_Kind
        :=
          (A_Less_Than_Operator, An_Equal_Operator, A_Greater_Than_Operator,
           A_Minus_Operator, A_Divide_Operator, A_Multiply_Operator,
           A_Concatenate_Operator, A_Plus_Operator,
           A_Less_Than_Or_Equal_Operator, A_Greater_Than_Or_Equal_Operator,
           A_Not_Equal_Operator, An_Exponentiate_Operator, An_Or_Operator,
           An_And_Operator, An_Xor_Operator, A_Mod_Operator, A_Rem_Operator,
           An_Abs_Operator, A_Not_Operator);

      Comp    : constant Gela.Compilations.Compilation_Access :=
        Node.Enclosing_Compilation;
      Token : constant Gela.Lexical_Types.Token :=
        Comp.Get_Token (Node.Operator_Symbol_Token);
   begin
      if Token.Symbol in Operator_Map'Range then
         Self.Result := Operator_Map (Token.Symbol);
      end if;
   end Operator_Symbol;

   overriding procedure Ordinary_Fixed_Point_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Ordinary_Fixed_Point_Definitions.
        Ordinary_Fixed_Point_Definition_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Ordinary_Fixed_Point_Definition;
   end Ordinary_Fixed_Point_Definition;

   overriding procedure Others_Choice
     (Self : in out Visiter;
      Node : not null Gela.Elements.Others_Choices.Others_Choice_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Others_Choice;
   end Others_Choice;

   overriding procedure Package_Body
     (Self : in out Visiter;
      Node : not null Gela.Elements.Package_Bodies.Package_Body_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Package_Body_Declaration;
   end Package_Body;

   overriding procedure Package_Body_Stub
     (Self : in out Visiter;
      Node : not null Gela.Elements.Package_Body_Stubs.
        Package_Body_Stub_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Package_Body_Stub;
   end Package_Body_Stub;

   overriding procedure Package_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Package_Declarations.
        Package_Declaration_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Package_Declaration;
   end Package_Declaration;

   overriding procedure Package_Instantiation
     (Self : in out Visiter;
      Node : not null Gela.Elements.Package_Instantiations.
        Package_Instantiation_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Package_Instantiation;
   end Package_Instantiation;

   overriding procedure Package_Renaming_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Package_Renaming_Declarations.
        Package_Renaming_Declaration_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Package_Renaming_Declaration;
   end Package_Renaming_Declaration;

   overriding procedure Parameter_Specification
     (Self : in out Visiter;
      Node : not null Gela.Elements.Parameter_Specifications.
        Parameter_Specification_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Parameter_Specification;
   end Parameter_Specification;

   overriding procedure Parenthesized_Expression
     (Self : in out Visiter;
      Node : not null Gela.Elements.Parenthesized_Expressions.
        Parenthesized_Expression_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Parenthesized_Expression;
   end Parenthesized_Expression;

   overriding procedure Pragma_Node
     (Self : in out Visiter;
      Node : not null Gela.Elements.Pragma_Nodes.Pragma_Node_Access)
   is
      package X renames Gela.Lexical_Types.Predefined_Symbols;

      Comp    : constant Gela.Compilations.Compilation_Access :=
        Node.Enclosing_Compilation;
      Token : constant Gela.Lexical_Types.Token :=
        Comp.Get_Token (Node.Pragma_Token);
      Map : constant array
        (Gela.Lexical_Types.Symbol range
           X.All_Calls_Remote .. X.Storage_Size) of Element_Flat_Kind :=
          (X.All_Calls_Remote => An_All_Calls_Remote_Pragma,
           X.Assert => An_Assert_Pragma,
           X.Assertion_Policy => An_Assertion_Policy_Pragma,
           X.Asynchronous => An_Asynchronous_Pragma,
           X.Atomic => An_Atomic_Pragma,
           X.Atomic_Components => An_Atomic_Components_Pragma,
           X.Attach_Handler => An_Attach_Handler_Pragma,
           X.Controlled => A_Controlled_Pragma,
           X.Convention => A_Convention_Pragma,
           X.Detect_Blocking => A_Detect_Blocking_Pragma,
           X.Discard_Names => A_Discard_Names_Pragma,
           X.Elaborate => An_Elaborate_Pragma,
           X.Elaborate_All => An_Elaborate_All_Pragma,
           X.Elaborate_Body => An_Elaborate_Body_Pragma,
           X.Export => An_Export_Pragma,
           X.Import => An_Import_Pragma,
           X.Inline => An_Inline_Pragma,
           X.Inspection_Point => An_Inspection_Point_Pragma,
           X.Interrupt_Handler => An_Interrupt_Handler_Pragma,
           X.Interrupt_Priority => An_Interrupt_Priority_Pragma,
           X.Linker_Options => A_Linker_Options_Pragma,
           X.List => A_List_Pragma,
           X.Locking_Policy => A_Locking_Policy_Pragma,
           X.No_Return => A_No_Return_Pragma,
           X.Normalize_Scalars => A_Normalize_Scalars_Pragma,
           X.Optimize => An_Optimize_Pragma,
           X.Pack => A_Pack_Pragma,
           X.Page => A_Page_Pragma,
           X.Partition_Elaboration_Policy =>
             A_Partition_Elaboration_Policy_Pragma,
           X.Preelaborable_Initialization =>
             A_Preelaborable_Initialization_Pragma,
           X.Preelaborate => A_Preelaborate_Pragma,
           X.Priority => A_Priority_Pragma,
           X.Priority_Specific_Dispatching =>
             A_Priority_Specific_Dispatching_Pragma,
           X.Profile => A_Profile_Pragma,
           X.Pure => A_Pure_Pragma,
           X.Queuing_Policy => A_Queuing_Policy_Pragma,
           X.Relative_Deadline => A_Relative_Deadline_Pragma,
           X.Remote_Call_Interface => A_Remote_Call_Interface_Pragma,
           X.Remote_Types => A_Remote_Types_Pragma,
           X.Restrictions => A_Restrictions_Pragma,
           X.Reviewable => A_Reviewable_Pragma,
           X.Shared_Passive => A_Shared_Passive_Pragma,
           X.Storage_Size => A_Storage_Size_Pragma,
           X.Suppress => A_Suppress_Pragma,
           X.Task_Dispatching_Policy => A_Task_Dispatching_Policy_Pragma,
           X.Unchecked_Union => An_Unchecked_Union_Pragma,
           X.Unsuppress => An_Unsuppress_Pragma,
           X.Volatile => A_Volatile_Pragma,
           X.Volatile_Components => A_Volatile_Components_Pragma,
           others => An_Unknown_Pragma);

   begin
      if Token.Symbol in Map'Range then
         Self.Result := Map (Token.Symbol);
      else
         Self.Result := An_Unknown_Pragma;
      end if;
   end Pragma_Node;

   overriding procedure Private_Type_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Private_Type_Declarations.
        Private_Type_Declaration_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Private_Type_Declaration;
   end Private_Type_Declaration;

   overriding procedure Private_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Private_Type_Definitions.
        Private_Type_Definition_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Private_Type_Definition;
   end Private_Type_Definition;

   --------------------
   -- Procedure_Body --
   --------------------

   overriding procedure Procedure_Body
     (Self : in out Visiter;
      Node : not null Gela.Elements.Procedure_Bodies.Procedure_Body_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Procedure_Body_Declaration;
   end Procedure_Body;

   ------------------------------
   -- Procedure_Call_Statement --
   ------------------------------

   overriding procedure Procedure_Call_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Procedure_Call_Statements.
        Procedure_Call_Statement_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Procedure_Call_Statement;
   end Procedure_Call_Statement;

   overriding procedure Procedure_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Procedure_Declarations.
        Procedure_Declaration_Access)
   is
      use type Gela.Lexical_Types.Token_Count;
   begin
      if Node.Renames_Token = 0 then
         Self.Result := A_Procedure_Declaration;
      else
         Self.Result := A_Procedure_Renaming_Declaration;
      end if;
   end Procedure_Declaration;

   overriding procedure Protected_Body
     (Self : in out Visiter;
      Node : not null Gela.Elements.Protected_Bodies.Protected_Body_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Protected_Body_Declaration;
   end Protected_Body;

   overriding procedure Protected_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Protected_Definitions.
        Protected_Definition_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Protected_Definition;
   end Protected_Definition;

   overriding procedure Qualified_Expression
     (Self : in out Visiter;
      Node : not null Gela.Elements.Qualified_Expressions.
        Qualified_Expression_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Qualified_Expression;
   end Qualified_Expression;

   overriding procedure Range_Attribute_Reference
     (Self : in out Visiter;
      Node : not null Gela.Elements.Range_Attribute_References.
        Range_Attribute_Reference_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Range_Attribute_Reference;
   end Range_Attribute_Reference;

   overriding procedure Record_Aggregate
     (Self : in out Visiter;
      Node : not null Gela.Elements.Record_Aggregates.Record_Aggregate_Access)
   is
      Comp  : constant Gela.Compilations.Compilation_Access :=
        Node.Enclosing_Compilation;

      IM : constant Gela.Interpretations.Interpretation_Manager_Access :=
        Comp.Context.Interpretation_Manager;

      TM : constant Gela.Type_Managers.Type_Manager_Access :=
        Comp.Context.Types;

      package Visiters is
         type Visiter is new Gela.Interpretations.Down_Visiter with record
            Result : Element_Flat_Kind := A_Record_Aggregate;
         end record;

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Kind   : Gela.Interpretations.Unknown_Auxiliary_Apply_Kinds;
            Down   : Gela.Interpretations.Interpretation_Index_Array);

      end Visiters;

      package body Visiters is

         overriding procedure On_Expression
           (Self   : in out Visiter;
            Tipe   : Gela.Semantic_Types.Type_Index;
            Kind   : Gela.Interpretations.Unknown_Auxiliary_Apply_Kinds;
            Down   : Gela.Interpretations.Interpretation_Index_Array)
         is
            pragma Unreferenced (Down, Kind);
            View : constant Gela.Types.Type_View_Access := TM.Get (Tipe);
         begin
            if View.Assigned and then View.Is_Array then
               Self.Result := A_Positional_Array_Aggregate;
            end if;
         end On_Expression;

      end Visiters;

      V : Visiters.Visiter;
   begin
      IM.Visit (Node.Down, V);
      Self.Result := V.Result;
   end Record_Aggregate;

   overriding procedure Record_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Record_Definitions.
        Record_Definition_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Record_Definition;
   end Record_Definition;

   overriding procedure Record_Representation_Clause
     (Self : in out Visiter;
      Node : not null Gela.Elements.Record_Representation_Clauses.
        Record_Representation_Clause_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Record_Representation_Clause;
   end Record_Representation_Clause;

   overriding procedure Record_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Record_Type_Definitions.
        Record_Type_Definition_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Record_Type_Definition;
   end Record_Type_Definition;

   overriding procedure Requeue_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Requeue_Statements.
        Requeue_Statement_Access)
   is
      use type Gela.Lexical_Types.Token_Count;
   begin
      if Node.With_Token = 0 then
         Self.Result := A_Requeue_Statement;
      else
         Self.Result := A_Requeue_Statement_With_Abort;
      end if;
   end Requeue_Statement;

   overriding procedure Scalar_Subtype_Indication
     (Self : in out Visiter;
      Node : not null Gela.Elements.Scalar_Subtype_Indications.
        Scalar_Subtype_Indication_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Subtype_Indication;
   end Scalar_Subtype_Indication;

   overriding procedure Select_Or_Path
     (Self : in out Visiter;
      Node : not null Gela.Elements.Select_Or_Paths.Select_Or_Path_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Or_Path;
   end Select_Or_Path;

   ------------------------
   -- Selected_Component --
   ------------------------

   overriding procedure Selected_Component
     (Self : in out Visiter;
      Node : not null Gela.Elements.Selected_Components.
        Selected_Component_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Selected_Component;
   end Selected_Component;

   -------------------------
   -- Selected_Identifier --
   -------------------------

   overriding procedure Selected_Identifier
     (Self : in out Visiter;
      Node : not null Gela.Elements.Selected_Identifiers.
        Selected_Identifier_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Selected_Component;
   end Selected_Identifier;

   overriding procedure Selective_Accept
     (Self : in out Visiter;
      Node : not null Gela.Elements.Selective_Accepts.Selective_Accept_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Selective_Accept_Statement;
   end Selective_Accept;

   overriding procedure Signed_Integer_Type_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Signed_Integer_Type_Definitions.
        Signed_Integer_Type_Definition_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Signed_Integer_Type_Definition;
   end Signed_Integer_Type_Definition;

   overriding procedure Simple_Expression_Range
     (Self : in out Visiter;
      Node : not null Gela.Elements.Simple_Expression_Ranges.
        Simple_Expression_Range_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Simple_Expression_Range;
   end Simple_Expression_Range;

   overriding procedure Simple_Expression_Range_Dr
     (Self : in out Visiter;
      Node : not null Gela.Elements.Simple_Expression_Range_Drs.
        Simple_Expression_Range_Dr_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Discrete_Simple_Expression_Range_DR;
   end Simple_Expression_Range_Dr;

   overriding procedure Simple_Return_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Simple_Return_Statements.
        Simple_Return_Statement_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Simple_Return_Statement;
   end Simple_Return_Statement;

   overriding procedure Single_Protected_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Single_Protected_Declarations.
        Single_Protected_Declaration_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Single_Protected_Declaration;
   end Single_Protected_Declaration;

   overriding procedure Single_Task_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Single_Task_Declarations.
        Single_Task_Declaration_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Single_Task_Declaration;
   end Single_Task_Declaration;

   --------------------
   -- String_Literal --
   --------------------

   overriding procedure String_Literal
     (Self : in out Visiter;
      Node : not null Gela.Elements.String_Literals.String_Literal_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_String_Literal;
   end String_Literal;

   overriding procedure Subtype_Declaration
     (Self : in out Visiter;
      Node : not null Gela.Elements.Subtype_Declarations.
        Subtype_Declaration_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Subtype_Declaration;
   end Subtype_Declaration;

   overriding procedure Task_Body
     (Self : in out Visiter;
      Node : not null Gela.Elements.Task_Bodies.Task_Body_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Task_Body_Declaration;
   end Task_Body;

   overriding procedure Task_Body_Stub
     (Self : in out Visiter;
      Node : not null Gela.Elements.Task_Body_Stubs.Task_Body_Stub_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Task_Body_Stub;
   end Task_Body_Stub;

   overriding procedure Task_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Task_Definitions.Task_Definition_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Task_Definition;
   end Task_Definition;

   overriding procedure Terminate_Alternative_Statement
     (Self : in out Visiter;
      Node : not null Gela.Elements.Terminate_Alternative_Statements.
        Terminate_Alternative_Statement_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Terminate_Alternative_Statement;
   end Terminate_Alternative_Statement;

   overriding procedure Unconstrained_Array_Definition
     (Self : in out Visiter;
      Node : not null Gela.Elements.Unconstrained_Array_Definitions.
        Unconstrained_Array_Definition_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := An_Unconstrained_Array_Definition;
   end Unconstrained_Array_Definition;

   ------------------------
   -- Use_Package_Clause --
   ------------------------

   overriding procedure Use_Package_Clause
     (Self : in out Visiter;
      Node : not null Gela.Elements.Use_Package_Clauses.
        Use_Package_Clause_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Use_Package_Clause;
   end Use_Package_Clause;

   overriding procedure Use_Type_Clause
     (Self : in out Visiter;
      Node : not null Gela.Elements.Use_Type_Clauses.Use_Type_Clause_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Use_Type_Clause;
   end Use_Type_Clause;

   overriding procedure Variant
     (Self : in out Visiter;
      Node : not null Gela.Elements.Variants.Variant_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Variant;
   end Variant;

   overriding procedure Variant_Part
     (Self : in out Visiter;
      Node : not null Gela.Elements.Variant_Parts.Variant_Part_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_Variant_Part;
   end Variant_Part;

   -----------------
   -- With_Clause --
   -----------------

   overriding procedure With_Clause
     (Self : in out Visiter;
      Node : not null Gela.Elements.With_Clauses.With_Clause_Access)
   is
      pragma Unreferenced (Node);
   begin
      Self.Result := A_With_Clause;
   end With_Clause;

end Asis.Extensions.Flat_Kinds;
