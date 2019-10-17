with League.Strings;

with Gela.Element_Visiters;
with Gela.Elements.Compilations;
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
with Gela.Elements.Associations;
with Gela.Elements.Association_Lists;
with Gela.Elements.Asynchronous_Selects;
with Gela.Elements.At_Clauses;
with Gela.Elements.Attribute_Definition_Clauses;
with Gela.Elements.Attribute_References;
with Gela.Elements.Block_Statements;
with Gela.Elements.Boxes;
with Gela.Elements.Case_Expressions;
with Gela.Elements.Case_Expression_Paths;
with Gela.Elements.Case_Paths;
with Gela.Elements.Case_Statements;
with Gela.Elements.Character_Literals;
with Gela.Elements.Choice_Parameter_Specifications;
with Gela.Elements.Compilation_Unit_Bodies;
with Gela.Elements.Compilation_Unit_Declarations;
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
with Gela.Elements.Defining_Operator_Symbols;
with Gela.Elements.Delay_Statements;
with Gela.Elements.Delta_Constraints;
with Gela.Elements.Derived_Record_Definitions;
with Gela.Elements.Derived_Type_Definitions;
with Gela.Elements.Digits_Constraints;
with Gela.Elements.Discrete_Range_Attribute_References;
with Gela.Elements.Discrete_Simple_Expression_Ranges;
with Gela.Elements.Discrete_Subtype_Indications;
with Gela.Elements.Discrete_Subtype_Indication_Drs;
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
with Gela.Elements.If_Expressions;
with Gela.Elements.If_Expression_Paths;
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
with Gela.Elements.Range_Attribute_References;
with Gela.Elements.Range_Attribute_Reference_Drs;
with Gela.Elements.Record_Aggregates;
with Gela.Elements.Record_Definitions;
with Gela.Elements.Record_Representation_Clauses;
with Gela.Elements.Record_Type_Definitions;
with Gela.Elements.Requeue_Statements;
with Gela.Elements.Return_Object_Specifications;
with Gela.Elements.Root_Type_Definitions;
with Gela.Elements.Scalar_Subtype_Indications;
with Gela.Elements.Select_Or_Paths;
with Gela.Elements.Selected_Components;
with Gela.Elements.Selected_Identifiers;
with Gela.Elements.Selective_Accepts;
with Gela.Elements.Short_Circuits;
with Gela.Elements.Signed_Integer_Type_Definitions;
with Gela.Elements.Simple_Expression_Ranges;
with Gela.Elements.Simple_Expression_Range_Drs;
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
with Gela.Elements.Variants;
with Gela.Elements.Variant_Parts;
with Gela.Elements.While_Loop_Statements;
with Gela.Elements.With_Clauses;

package Element_Printers is

   type Element_Printer is new Gela.Element_Visiters.Visiter with private;
   
   function Image (Self : Element_Printer)
     return League.Strings.Universal_String;

private
   type Element_Printer is new Gela.Element_Visiters.Visiter with record
      Image : League.Strings.Universal_String;
   end record;

   overriding procedure Compilation
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Compilations.Compilation_Access);

   overriding procedure Abort_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Abort_Statements.Abort_Statement_Access);

   overriding procedure Accept_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Accept_Statements.Accept_Statement_Access);

   overriding procedure Access_To_Function_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Access_To_Function_Definitions.
        Access_To_Function_Definition_Access);

   overriding procedure Access_To_Object_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Access_To_Object_Definitions.
        Access_To_Object_Definition_Access);

   overriding procedure Access_To_Procedure_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Access_To_Procedure_Definitions.
        Access_To_Procedure_Definition_Access);

   overriding procedure Allocator
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Allocators.Allocator_Access);

   overriding procedure Anonymous_Access_To_Function_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Anonymous_Access_To_Function_Definitions.
        Anonymous_Access_To_Function_Definition_Access);

   overriding procedure Anonymous_Access_To_Object_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Anonymous_Access_To_Object_Definitions.
        Anonymous_Access_To_Object_Definition_Access);

   overriding procedure Anonymous_Access_To_Procedure_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Anonymous_Access_To_Procedure_Definitions.
        Anonymous_Access_To_Procedure_Definition_Access);

   overriding procedure Aspect_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Aspect_Specifications.
        Aspect_Specification_Access);

   overriding procedure Assignment_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Assignment_Statements.
        Assignment_Statement_Access);

   overriding procedure Association
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Associations.Association_Access);

   overriding procedure Association_List
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Association_Lists.Association_List_Access);

   overriding procedure Asynchronous_Select
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Asynchronous_Selects.
        Asynchronous_Select_Access);

   overriding procedure At_Clause
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.At_Clauses.At_Clause_Access);

   overriding procedure Attribute_Definition_Clause
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Attribute_Definition_Clauses.
        Attribute_Definition_Clause_Access);

   overriding procedure Attribute_Reference
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Attribute_References.
        Attribute_Reference_Access);

   overriding procedure Block_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Block_Statements.Block_Statement_Access);

   overriding procedure Box
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Boxes.Box_Access);

   overriding procedure Case_Expression
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Case_Expressions.Case_Expression_Access);

   overriding procedure Case_Expression_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Case_Expression_Paths.
        Case_Expression_Path_Access);

   overriding procedure Case_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Case_Paths.Case_Path_Access);

   overriding procedure Case_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Case_Statements.Case_Statement_Access);

   overriding procedure Character_Literal
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Character_Literals.
        Character_Literal_Access);

   overriding procedure Choice_Parameter_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Choice_Parameter_Specifications.
        Choice_Parameter_Specification_Access);

   overriding procedure Compilation_Unit_Body
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Compilation_Unit_Bodies.
        Compilation_Unit_Body_Access);

   overriding procedure Compilation_Unit_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Compilation_Unit_Declarations.
        Compilation_Unit_Declaration_Access);

   overriding procedure Component_Clause
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Component_Clauses.Component_Clause_Access);

   overriding procedure Component_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Component_Declarations.
        Component_Declaration_Access);

   overriding procedure Component_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Component_Definitions.
        Component_Definition_Access);

   overriding procedure Composite_Constraint
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Composite_Constraints.
        Composite_Constraint_Access);

   overriding procedure Composite_Subtype_Indication
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Composite_Subtype_Indications.
        Composite_Subtype_Indication_Access);

   overriding procedure Constrained_Array_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Constrained_Array_Definitions.
        Constrained_Array_Definition_Access);

   overriding procedure Decimal_Fixed_Point_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Decimal_Fixed_Point_Definitions.
        Decimal_Fixed_Point_Definition_Access);

   overriding procedure Defining_Character_Literal
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Defining_Character_Literals.
        Defining_Character_Literal_Access);

   overriding procedure Defining_Enumeration_Literal
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Defining_Enumeration_Literals.
        Defining_Enumeration_Literal_Access);

   overriding procedure Defining_Expanded_Unit_Name
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Defining_Expanded_Unit_Names.
        Defining_Expanded_Unit_Name_Access);

   overriding procedure Defining_Identifier
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Defining_Identifiers.
        Defining_Identifier_Access);

   overriding procedure Defining_Operator_Symbol
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Defining_Operator_Symbols.
        Defining_Operator_Symbol_Access);

   overriding procedure Delay_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Delay_Statements.Delay_Statement_Access);

   overriding procedure Delta_Constraint
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Delta_Constraints.Delta_Constraint_Access);

   overriding procedure Derived_Record_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Derived_Record_Definitions.
        Derived_Record_Definition_Access);

   overriding procedure Derived_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Derived_Type_Definitions.
        Derived_Type_Definition_Access);

   overriding procedure Digits_Constraint
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Digits_Constraints.
        Digits_Constraint_Access);

   overriding procedure Discrete_Range_Attribute_Reference
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Discrete_Range_Attribute_References.
        Discrete_Range_Attribute_Reference_Access);

   overriding procedure Discrete_Simple_Expression_Range
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Discrete_Simple_Expression_Ranges.
        Discrete_Simple_Expression_Range_Access);

   overriding procedure Discrete_Subtype_Indication
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Discrete_Subtype_Indications.
        Discrete_Subtype_Indication_Access);

   overriding procedure Discrete_Subtype_Indication_Dr
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Discrete_Subtype_Indication_Drs.
        Discrete_Subtype_Indication_Dr_Access);

   overriding procedure Discriminant_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Discriminant_Specifications.
        Discriminant_Specification_Access);

   overriding procedure Element_Iterator_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Element_Iterator_Specifications.
        Element_Iterator_Specification_Access);

   overriding procedure Else_Expression_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Else_Expression_Paths.
        Else_Expression_Path_Access);

   overriding procedure Else_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Else_Paths.Else_Path_Access);

   overriding procedure Elsif_Expression_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Elsif_Expression_Paths.
        Elsif_Expression_Path_Access);

   overriding procedure Elsif_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Elsif_Paths.Elsif_Path_Access);

   overriding procedure Entry_Body
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Entry_Bodies.Entry_Body_Access);

   overriding procedure Entry_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Entry_Declarations.
        Entry_Declaration_Access);

   overriding procedure Entry_Index_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Entry_Index_Specifications.
        Entry_Index_Specification_Access);

   overriding procedure Enumeration_Literal_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Enumeration_Literal_Specifications.
        Enumeration_Literal_Specification_Access);

   overriding procedure Enumeration_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Enumeration_Type_Definitions.
        Enumeration_Type_Definition_Access);

   overriding procedure Exception_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Exception_Declarations.
        Exception_Declaration_Access);

   overriding procedure Exception_Handler
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Exception_Handlers.
        Exception_Handler_Access);

   overriding procedure Exception_Renaming_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Exception_Renaming_Declarations.
        Exception_Renaming_Declaration_Access);

   overriding procedure Exit_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Exit_Statements.Exit_Statement_Access);

   overriding procedure Explicit_Dereference
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Explicit_Dereferences.
        Explicit_Dereference_Access);

   overriding procedure Extended_Return_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Extended_Return_Statements.
        Extended_Return_Statement_Access);

   overriding procedure Extension_Aggregate
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Extension_Aggregates.
        Extension_Aggregate_Access);

   overriding procedure Floating_Point_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Floating_Point_Definitions.
        Floating_Point_Definition_Access);

   overriding procedure For_Loop_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.For_Loop_Statements.
        For_Loop_Statement_Access);

   overriding procedure Formal_Access_To_Function_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Access_To_Function_Definitions.
        Formal_Access_To_Function_Definition_Access);

   overriding procedure Formal_Access_To_Object_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Access_To_Object_Definitions.
        Formal_Access_To_Object_Definition_Access);

   overriding procedure Formal_Access_To_Procedure_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Access_To_Procedure_Definitions.
        Formal_Access_To_Procedure_Definition_Access);

   overriding procedure Formal_Constrained_Array_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Constrained_Array_Definitions.
        Formal_Constrained_Array_Definition_Access);

   overriding procedure Formal_Decimal_Fixed_Point_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Decimal_Fixed_Point_Definitions.
        Formal_Decimal_Fixed_Point_Definition_Access);

   overriding procedure Formal_Derived_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Derived_Type_Definitions.
        Formal_Derived_Type_Definition_Access);

   overriding procedure Formal_Discrete_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Discrete_Type_Definitions.
        Formal_Discrete_Type_Definition_Access);

   overriding procedure Formal_Floating_Point_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Floating_Point_Definitions.
        Formal_Floating_Point_Definition_Access);

   overriding procedure Formal_Function_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Function_Declarations.
        Formal_Function_Declaration_Access);

   overriding procedure Formal_Incomplete_Type_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Incomplete_Type_Declarations.
        Formal_Incomplete_Type_Declaration_Access);

   overriding procedure Formal_Interface_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Interface_Type_Definitions.
        Formal_Interface_Type_Definition_Access);

   overriding procedure Formal_Modular_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Modular_Type_Definitions.
        Formal_Modular_Type_Definition_Access);

   overriding procedure Formal_Object_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Object_Declarations.
        Formal_Object_Declaration_Access);

   overriding procedure Formal_Ordinary_Fixed_Point_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Ordinary_Fixed_Point_Definitions.
        Formal_Ordinary_Fixed_Point_Definition_Access);

   overriding procedure Formal_Package_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Package_Declarations.
        Formal_Package_Declaration_Access);

   overriding procedure Formal_Private_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Private_Type_Definitions.
        Formal_Private_Type_Definition_Access);

   overriding procedure Formal_Procedure_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Procedure_Declarations.
        Formal_Procedure_Declaration_Access);

   overriding procedure Formal_Signed_Integer_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Signed_Integer_Type_Definitions.
        Formal_Signed_Integer_Type_Definition_Access);

   overriding procedure Formal_Type_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Type_Declarations.
        Formal_Type_Declaration_Access);

   overriding procedure Formal_Unconstrained_Array_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Unconstrained_Array_Definitions.
        Formal_Unconstrained_Array_Definition_Access);

   overriding procedure Full_Type_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Full_Type_Declarations.
        Full_Type_Declaration_Access);

   overriding procedure Function_Body
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Function_Bodies.Function_Body_Access);

   overriding procedure Function_Call
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Function_Calls.Function_Call_Access);

   overriding procedure Function_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Function_Declarations.
        Function_Declaration_Access);

   overriding procedure Function_Instantiation
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Function_Instantiations.
        Function_Instantiation_Access);

   overriding procedure Generalized_Iterator_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Generalized_Iterator_Specifications.
        Generalized_Iterator_Specification_Access);

   overriding procedure Generic_Association
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Generic_Associations.
        Generic_Association_Access);

   overriding procedure Generic_Function_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Generic_Function_Declarations.
        Generic_Function_Declaration_Access);

   overriding procedure Generic_Function_Renaming
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Generic_Function_Renamings.
        Generic_Function_Renaming_Access);

   overriding procedure Generic_Package_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Generic_Package_Declarations.
        Generic_Package_Declaration_Access);

   overriding procedure Generic_Package_Renaming
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Generic_Package_Renamings.
        Generic_Package_Renaming_Access);

   overriding procedure Generic_Procedure_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Generic_Procedure_Declarations.
        Generic_Procedure_Declaration_Access);

   overriding procedure Generic_Procedure_Renaming
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Generic_Procedure_Renamings.
        Generic_Procedure_Renaming_Access);

   overriding procedure Goto_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Goto_Statements.Goto_Statement_Access);

   overriding procedure Identifier
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Identifiers.Identifier_Access);

   overriding procedure If_Expression
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.If_Expressions.If_Expression_Access);

   overriding procedure If_Expression_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.If_Expression_Paths.
        If_Expression_Path_Access);

   overriding procedure If_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.If_Paths.If_Path_Access);

   overriding procedure If_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.If_Statements.If_Statement_Access);

   overriding procedure Incomplete_Type_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Incomplete_Type_Declarations.
        Incomplete_Type_Declaration_Access);

   overriding procedure Incomplete_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Incomplete_Type_Definitions.
        Incomplete_Type_Definition_Access);

   overriding procedure Interface_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Interface_Type_Definitions.
        Interface_Type_Definition_Access);

   overriding procedure Known_Discriminant_Part
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Known_Discriminant_Parts.
        Known_Discriminant_Part_Access);

   overriding procedure Label_Decorator
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Label_Decorators.Label_Decorator_Access);

   overriding procedure Loop_Parameter_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Loop_Parameter_Specifications.
        Loop_Parameter_Specification_Access);

   overriding procedure Loop_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Loop_Statements.Loop_Statement_Access);

   overriding procedure Membership_Test
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Membership_Tests.Membership_Test_Access);

   overriding procedure Modular_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Modular_Type_Definitions.
        Modular_Type_Definition_Access);

   overriding procedure Null_Component
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Null_Components.Null_Component_Access);

   overriding procedure Null_Literal
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Null_Literals.Null_Literal_Access);

   overriding procedure Null_Record_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Null_Record_Definitions.
        Null_Record_Definition_Access);

   overriding procedure Null_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Null_Statements.Null_Statement_Access);

   overriding procedure Number_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Number_Declarations.
        Number_Declaration_Access);

   overriding procedure Numeric_Literal
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Numeric_Literals.Numeric_Literal_Access);

   overriding procedure Object_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Object_Declarations.
        Object_Declaration_Access);

   overriding procedure Object_Renaming_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Object_Renaming_Declarations.
        Object_Renaming_Declaration_Access);

   overriding procedure Operator_Symbol
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Operator_Symbols.Operator_Symbol_Access);

   overriding procedure Ordinary_Fixed_Point_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Ordinary_Fixed_Point_Definitions.
        Ordinary_Fixed_Point_Definition_Access);

   overriding procedure Others_Choice
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Others_Choices.Others_Choice_Access);

   overriding procedure Package_Body
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Package_Bodies.Package_Body_Access);

   overriding procedure Package_Body_Stub
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Package_Body_Stubs.
        Package_Body_Stub_Access);

   overriding procedure Package_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Package_Declarations.
        Package_Declaration_Access);

   overriding procedure Package_Instantiation
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Package_Instantiations.
        Package_Instantiation_Access);

   overriding procedure Package_Renaming_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Package_Renaming_Declarations.
        Package_Renaming_Declaration_Access);

   overriding procedure Parameter_Association
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Parameter_Associations.
        Parameter_Association_Access);

   overriding procedure Parameter_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Parameter_Specifications.
        Parameter_Specification_Access);

   overriding procedure Parenthesized_Expression
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Parenthesized_Expressions.
        Parenthesized_Expression_Access);

   overriding procedure Pragma_Argument_Association
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Pragma_Argument_Associations.
        Pragma_Argument_Association_Access);

   overriding procedure Pragma_Node
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Pragma_Nodes.Pragma_Node_Access);

   overriding procedure Private_Extension_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Private_Extension_Declarations.
        Private_Extension_Declaration_Access);

   overriding procedure Private_Extension_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Private_Extension_Definitions.
        Private_Extension_Definition_Access);

   overriding procedure Private_Type_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Private_Type_Declarations.
        Private_Type_Declaration_Access);

   overriding procedure Private_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Private_Type_Definitions.
        Private_Type_Definition_Access);

   overriding procedure Procedure_Body
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Procedure_Bodies.Procedure_Body_Access);

   overriding procedure Procedure_Call_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Procedure_Call_Statements.
        Procedure_Call_Statement_Access);

   overriding procedure Procedure_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Procedure_Declarations.
        Procedure_Declaration_Access);

   overriding procedure Procedure_Instantiation
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Procedure_Instantiations.
        Procedure_Instantiation_Access);

   overriding procedure Protected_Body
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Protected_Bodies.Protected_Body_Access);

   overriding procedure Protected_Body_Stub
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Protected_Body_Stubs.
        Protected_Body_Stub_Access);

   overriding procedure Protected_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Protected_Definitions.
        Protected_Definition_Access);

   overriding procedure Protected_Type_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Protected_Type_Declarations.
        Protected_Type_Declaration_Access);

   overriding procedure Qualified_Expression
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Qualified_Expressions.
        Qualified_Expression_Access);

   overriding procedure Quantified_Expression
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Quantified_Expressions.
        Quantified_Expression_Access);

   overriding procedure Raise_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Raise_Statements.Raise_Statement_Access);

   overriding procedure Range_Attribute_Reference
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Range_Attribute_References.
        Range_Attribute_Reference_Access);

   overriding procedure Range_Attribute_Reference_Dr
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Range_Attribute_Reference_Drs.
        Range_Attribute_Reference_Dr_Access);

   overriding procedure Record_Aggregate
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Record_Aggregates.Record_Aggregate_Access);

   overriding procedure Record_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Record_Definitions.
        Record_Definition_Access);

   overriding procedure Record_Representation_Clause
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Record_Representation_Clauses.
        Record_Representation_Clause_Access);

   overriding procedure Record_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Record_Type_Definitions.
        Record_Type_Definition_Access);

   overriding procedure Requeue_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Requeue_Statements.
        Requeue_Statement_Access);

   overriding procedure Return_Object_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Return_Object_Specifications.
        Return_Object_Specification_Access);

   overriding procedure Root_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Root_Type_Definitions.
        Root_Type_Definition_Access);

   overriding procedure Scalar_Subtype_Indication
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Scalar_Subtype_Indications.
        Scalar_Subtype_Indication_Access);

   overriding procedure Select_Or_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Select_Or_Paths.Select_Or_Path_Access);

   overriding procedure Selected_Component
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Selected_Components.
        Selected_Component_Access);

   overriding procedure Selected_Identifier
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Selected_Identifiers.
        Selected_Identifier_Access);

   overriding procedure Selective_Accept
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Selective_Accepts.Selective_Accept_Access);

   overriding procedure Short_Circuit
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Short_Circuits.Short_Circuit_Access);

   overriding procedure Signed_Integer_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Signed_Integer_Type_Definitions.
        Signed_Integer_Type_Definition_Access);

   overriding procedure Simple_Expression_Range
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Simple_Expression_Ranges.
        Simple_Expression_Range_Access);

   overriding procedure Simple_Expression_Range_Dr
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Simple_Expression_Range_Drs.
        Simple_Expression_Range_Dr_Access);

   overriding procedure Simple_Return_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Simple_Return_Statements.
        Simple_Return_Statement_Access);

   overriding procedure Single_Protected_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Single_Protected_Declarations.
        Single_Protected_Declaration_Access);

   overriding procedure Single_Task_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Single_Task_Declarations.
        Single_Task_Declaration_Access);

   overriding procedure String_Literal
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.String_Literals.String_Literal_Access);

   overriding procedure Subtype_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Subtype_Declarations.
        Subtype_Declaration_Access);

   overriding procedure Subunit
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Subunits.Subunit_Access);

   overriding procedure Task_Body
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Task_Bodies.Task_Body_Access);

   overriding procedure Task_Body_Stub
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Task_Body_Stubs.Task_Body_Stub_Access);

   overriding procedure Task_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Task_Definitions.Task_Definition_Access);

   overriding procedure Task_Type_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Task_Type_Declarations.
        Task_Type_Declaration_Access);

   overriding procedure Terminate_Alternative_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Terminate_Alternative_Statements.
        Terminate_Alternative_Statement_Access);

   overriding procedure Then_Abort_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Then_Abort_Paths.Then_Abort_Path_Access);

   overriding procedure Unconstrained_Array_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Unconstrained_Array_Definitions.
        Unconstrained_Array_Definition_Access);

   overriding procedure Unknown_Discriminant_Part
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Unknown_Discriminant_Parts.
        Unknown_Discriminant_Part_Access);

   overriding procedure Use_Package_Clause
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Use_Package_Clauses.
        Use_Package_Clause_Access);

   overriding procedure Use_Type_Clause
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Use_Type_Clauses.Use_Type_Clause_Access);

   overriding procedure Variant
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Variants.Variant_Access);

   overriding procedure Variant_Part
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Variant_Parts.Variant_Part_Access);

   overriding procedure While_Loop_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.While_Loop_Statements.
        While_Loop_Statement_Access);

   overriding procedure With_Clause
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.With_Clauses.With_Clause_Access);
   
end Element_Printers;
