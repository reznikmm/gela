package body Element_Printers is

   -----------
   -- Image --
   -----------

   function Image
     (Self : Element_Printer) return League.Strings.Universal_String is
   begin
      return Self.Image;
   end Image;

   -----------------
   -- Compilation --
   -----------------

   overriding procedure Compilation
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Compilations.Compilation_Access)
   is
   begin
      Self.Image.Append ("Compilation");
   end Compilation;

   ---------------------
   -- Abort_Statement --
   ---------------------

   overriding procedure Abort_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Abort_Statements.Abort_Statement_Access)
   is
   begin
      Self.Image.Append ("Abort_Statement");
   end Abort_Statement;

   ----------------------
   -- Accept_Statement --
   ----------------------

   overriding procedure Accept_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Accept_Statements.Accept_Statement_Access)
   is
   begin
      Self.Image.Append ("Accept_Statement");
   end Accept_Statement;

   -----------------------------------
   -- Access_To_Function_Definition --
   -----------------------------------

   overriding procedure Access_To_Function_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Access_To_Function_Definitions
        .Access_To_Function_Definition_Access)
   is
   begin
      Self.Image.Append ("Access_To_Function_Definition");
   end Access_To_Function_Definition;

   ---------------------------------
   -- Access_To_Object_Definition --
   ---------------------------------

   overriding procedure Access_To_Object_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Access_To_Object_Definitions
        .Access_To_Object_Definition_Access)
   is
   begin
      Self.Image.Append ("Access_To_Object_Definition");
   end Access_To_Object_Definition;

   ------------------------------------
   -- Access_To_Procedure_Definition --
   ------------------------------------

   overriding procedure Access_To_Procedure_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Access_To_Procedure_Definitions
        .Access_To_Procedure_Definition_Access)
   is
   begin
      Self.Image.Append ("Access_To_Procedure_Definition");
   end Access_To_Procedure_Definition;

   ---------------
   -- Allocator --
   ---------------

   overriding procedure Allocator
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Allocators.Allocator_Access)
   is
   begin
      Self.Image.Append ("Allocator");
   end Allocator;

   ---------------------------------------------
   -- Anonymous_Access_To_Function_Definition --
   ---------------------------------------------

   overriding procedure Anonymous_Access_To_Function_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Anonymous_Access_To_Function_Definitions
        .Anonymous_Access_To_Function_Definition_Access)
   is
   begin
      Self.Image.Append ("Anonymous_Access_To_Function_Definition");
   end Anonymous_Access_To_Function_Definition;

   -------------------------------------------
   -- Anonymous_Access_To_Object_Definition --
   -------------------------------------------

   overriding procedure Anonymous_Access_To_Object_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Anonymous_Access_To_Object_Definitions
        .Anonymous_Access_To_Object_Definition_Access)
   is
   begin
      Self.Image.Append ("Anonymous_Access_To_Object_Definition");
   end Anonymous_Access_To_Object_Definition;

   ----------------------------------------------
   -- Anonymous_Access_To_Procedure_Definition --
   ----------------------------------------------

   overriding procedure Anonymous_Access_To_Procedure_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Anonymous_Access_To_Procedure_Definitions
        .Anonymous_Access_To_Procedure_Definition_Access)
   is
   begin
      Self.Image.Append ("Anonymous_Access_To_Procedure_Definition");
   end Anonymous_Access_To_Procedure_Definition;

   --------------------------
   -- Aspect_Specification --
   --------------------------

   overriding procedure Aspect_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Aspect_Specifications
        .Aspect_Specification_Access)
   is
   begin
      Self.Image.Append ("Aspect_Specification");
   end Aspect_Specification;

   --------------------------
   -- Assignment_Statement --
   --------------------------

   overriding procedure Assignment_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Assignment_Statements
        .Assignment_Statement_Access)
   is
   begin
      Self.Image.Append ("Assignment_Statement");
   end Assignment_Statement;

   -----------------
   -- Association --
   -----------------

   overriding procedure Association
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Associations.Association_Access)
   is
   begin
      Self.Image.Append ("Association");
   end Association;

   ----------------------
   -- Association_List --
   ----------------------

   overriding procedure Association_List
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Association_Lists.Association_List_Access)
   is
   begin
      Self.Image.Append ("Association_List");
   end Association_List;

   -------------------------
   -- Asynchronous_Select --
   -------------------------

   overriding procedure Asynchronous_Select
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Asynchronous_Selects
        .Asynchronous_Select_Access)
   is
   begin
      Self.Image.Append ("Asynchronous_Select");
   end Asynchronous_Select;

   ---------------
   -- At_Clause --
   ---------------

   overriding procedure At_Clause
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.At_Clauses.At_Clause_Access)
   is
   begin
      Self.Image.Append ("At_Clause");
   end At_Clause;

   ---------------------------------
   -- Attribute_Definition_Clause --
   ---------------------------------

   overriding procedure Attribute_Definition_Clause
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Attribute_Definition_Clauses
        .Attribute_Definition_Clause_Access)
   is
   begin
      Self.Image.Append ("Attribute_Definition_Clause");
   end Attribute_Definition_Clause;

   -------------------------
   -- Attribute_Reference --
   -------------------------

   overriding procedure Attribute_Reference
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Attribute_References
        .Attribute_Reference_Access)
   is
   begin
      Self.Image.Append ("Attribute_Reference");
   end Attribute_Reference;

   ---------------------
   -- Block_Statement --
   ---------------------

   overriding procedure Block_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Block_Statements.Block_Statement_Access)
   is
   begin
      Self.Image.Append ("Block_Statement");
   end Block_Statement;

   ---------
   -- Box --
   ---------

   overriding procedure Box
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Boxes.Box_Access)
   is
   begin
      Self.Image.Append ("Box");
   end Box;

   ---------------------
   -- Case_Expression --
   ---------------------

   overriding procedure Case_Expression
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Case_Expressions.Case_Expression_Access)
   is
   begin
      Self.Image.Append ("Case_Expression");
   end Case_Expression;

   --------------------------
   -- Case_Expression_Path --
   --------------------------

   overriding procedure Case_Expression_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Case_Expression_Paths
        .Case_Expression_Path_Access)
   is
   begin
      Self.Image.Append ("Case_Expression_Path");
   end Case_Expression_Path;

   ---------------
   -- Case_Path --
   ---------------

   overriding procedure Case_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Case_Paths.Case_Path_Access)
   is
   begin
      Self.Image.Append ("Case_Path");
   end Case_Path;

   --------------------
   -- Case_Statement --
   --------------------

   overriding procedure Case_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Case_Statements.Case_Statement_Access)
   is
   begin
      Self.Image.Append ("Case_Statement");
   end Case_Statement;

   -----------------------
   -- Character_Literal --
   -----------------------

   overriding procedure Character_Literal
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Character_Literals
        .Character_Literal_Access)
   is
   begin
      Self.Image.Append ("Character_Literal");
   end Character_Literal;

   ------------------------------------
   -- Choice_Parameter_Specification --
   ------------------------------------

   overriding procedure Choice_Parameter_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Choice_Parameter_Specifications
        .Choice_Parameter_Specification_Access)
   is
   begin
      Self.Image.Append ("Choice_Parameter_Specification");
   end Choice_Parameter_Specification;

   ---------------------------
   -- Compilation_Unit_Body --
   ---------------------------

   overriding procedure Compilation_Unit_Body
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Compilation_Unit_Bodies
        .Compilation_Unit_Body_Access)
   is
   begin
      Self.Image.Append ("Compilation_Unit_Body");
   end Compilation_Unit_Body;

   ----------------------------------
   -- Compilation_Unit_Declaration --
   ----------------------------------

   overriding procedure Compilation_Unit_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Compilation_Unit_Declarations
        .Compilation_Unit_Declaration_Access)
   is
   begin
      Self.Image.Append ("Compilation_Unit_Declaration");
   end Compilation_Unit_Declaration;

   ----------------------
   -- Component_Clause --
   ----------------------

   overriding procedure Component_Clause
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Component_Clauses.Component_Clause_Access)
   is
   begin
      Self.Image.Append ("Component_Clause");
   end Component_Clause;

   ---------------------------
   -- Component_Declaration --
   ---------------------------

   overriding procedure Component_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Component_Declarations
        .Component_Declaration_Access)
   is
   begin
      Self.Image.Append ("Component_Declaration");
   end Component_Declaration;

   --------------------------
   -- Component_Definition --
   --------------------------

   overriding procedure Component_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Component_Definitions
        .Component_Definition_Access)
   is
   begin
      Self.Image.Append ("Component_Definition");
   end Component_Definition;

   --------------------------
   -- Composite_Constraint --
   --------------------------

   overriding procedure Composite_Constraint
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Composite_Constraints
        .Composite_Constraint_Access)
   is
   begin
      Self.Image.Append ("Composite_Constraint");
   end Composite_Constraint;

   ----------------------------------
   -- Composite_Subtype_Indication --
   ----------------------------------

   overriding procedure Composite_Subtype_Indication
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Composite_Subtype_Indications
        .Composite_Subtype_Indication_Access)
   is
   begin
      Self.Image.Append ("Composite_Subtype_Indication");
   end Composite_Subtype_Indication;

   ----------------------------------
   -- Constrained_Array_Definition --
   ----------------------------------

   overriding procedure Constrained_Array_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Constrained_Array_Definitions
        .Constrained_Array_Definition_Access)
   is
   begin
      Self.Image.Append ("Constrained_Array_Definition");
   end Constrained_Array_Definition;

   ------------------------------------
   -- Decimal_Fixed_Point_Definition --
   ------------------------------------

   overriding procedure Decimal_Fixed_Point_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Decimal_Fixed_Point_Definitions
        .Decimal_Fixed_Point_Definition_Access)
   is
   begin
      Self.Image.Append ("Decimal_Fixed_Point_Definition");
   end Decimal_Fixed_Point_Definition;

   --------------------------------
   -- Defining_Character_Literal --
   --------------------------------

   overriding procedure Defining_Character_Literal
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Defining_Character_Literals
        .Defining_Character_Literal_Access)
   is
   begin
      Self.Image.Append ("Defining_Character_Literal");
   end Defining_Character_Literal;

   ----------------------------------
   -- Defining_Enumeration_Literal --
   ----------------------------------

   overriding procedure Defining_Enumeration_Literal
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Defining_Enumeration_Literals
        .Defining_Enumeration_Literal_Access)
   is
   begin
      Self.Image.Append ("Defining_Enumeration_Literal");
   end Defining_Enumeration_Literal;

   ---------------------------------
   -- Defining_Expanded_Unit_Name --
   ---------------------------------

   overriding procedure Defining_Expanded_Unit_Name
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Defining_Expanded_Unit_Names
        .Defining_Expanded_Unit_Name_Access)
   is
   begin
      Self.Image.Append ("Defining_Expanded_Unit_Name");
   end Defining_Expanded_Unit_Name;

   -------------------------
   -- Defining_Identifier --
   -------------------------

   overriding procedure Defining_Identifier
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Defining_Identifiers
        .Defining_Identifier_Access)
   is
   begin
      Self.Image.Append ("Defining_Identifier");
   end Defining_Identifier;

   ------------------------------
   -- Defining_Operator_Symbol --
   ------------------------------

   overriding procedure Defining_Operator_Symbol
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Defining_Operator_Symbols
        .Defining_Operator_Symbol_Access)
   is
   begin
      Self.Image.Append ("Defining_Operator_Symbol");
   end Defining_Operator_Symbol;

   ---------------------
   -- Delay_Statement --
   ---------------------

   overriding procedure Delay_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Delay_Statements.Delay_Statement_Access)
   is
   begin
      Self.Image.Append ("Delay_Statement");
   end Delay_Statement;

   ----------------------
   -- Delta_Constraint --
   ----------------------

   overriding procedure Delta_Constraint
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Delta_Constraints.Delta_Constraint_Access)
   is
   begin
      Self.Image.Append ("Delta_Constraint");
   end Delta_Constraint;

   -------------------------------
   -- Derived_Record_Definition --
   -------------------------------

   overriding procedure Derived_Record_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Derived_Record_Definitions
        .Derived_Record_Definition_Access)
   is
   begin
      Self.Image.Append ("Derived_Record_Definition");
   end Derived_Record_Definition;

   -----------------------------
   -- Derived_Type_Definition --
   -----------------------------

   overriding procedure Derived_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Derived_Type_Definitions
        .Derived_Type_Definition_Access)
   is
   begin
      Self.Image.Append ("Derived_Type_Definition");
   end Derived_Type_Definition;

   -----------------------
   -- Digits_Constraint --
   -----------------------

   overriding procedure Digits_Constraint
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Digits_Constraints
        .Digits_Constraint_Access)
   is
   begin
      Self.Image.Append ("Digits_Constraint");
   end Digits_Constraint;

   ----------------------------------------
   -- Discrete_Range_Attribute_Reference --
   ----------------------------------------

   overriding procedure Discrete_Range_Attribute_Reference
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Discrete_Range_Attribute_References
        .Discrete_Range_Attribute_Reference_Access)
   is
   begin
      Self.Image.Append ("Discrete_Range_Attribute_Reference");
   end Discrete_Range_Attribute_Reference;

   --------------------------------------
   -- Discrete_Simple_Expression_Range --
   --------------------------------------

   overriding procedure Discrete_Simple_Expression_Range
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Discrete_Simple_Expression_Ranges
        .Discrete_Simple_Expression_Range_Access)
   is
   begin
      Self.Image.Append ("Discrete_Simple_Expression_Range");
   end Discrete_Simple_Expression_Range;

   ---------------------------------
   -- Discrete_Subtype_Indication --
   ---------------------------------

   overriding procedure Discrete_Subtype_Indication
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Discrete_Subtype_Indications
        .Discrete_Subtype_Indication_Access)
   is
   begin
      Self.Image.Append ("Discrete_Subtype_Indication");
   end Discrete_Subtype_Indication;

   ------------------------------------
   -- Discrete_Subtype_Indication_Dr --
   ------------------------------------

   overriding procedure Discrete_Subtype_Indication_Dr
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Discrete_Subtype_Indication_Drs
        .Discrete_Subtype_Indication_Dr_Access)
   is
   begin
      Self.Image.Append ("Discrete_Subtype_Indication_Dr");
   end Discrete_Subtype_Indication_Dr;

   --------------------------------
   -- Discriminant_Specification --
   --------------------------------

   overriding procedure Discriminant_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Discriminant_Specifications
        .Discriminant_Specification_Access)
   is
   begin
      Self.Image.Append ("Discriminant_Specification");
   end Discriminant_Specification;

   ------------------------------------
   -- Element_Iterator_Specification --
   ------------------------------------

   overriding procedure Element_Iterator_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Element_Iterator_Specifications
        .Element_Iterator_Specification_Access)
   is
   begin
      Self.Image.Append ("Element_Iterator_Specification");
   end Element_Iterator_Specification;

   --------------------------
   -- Else_Expression_Path --
   --------------------------

   overriding procedure Else_Expression_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Else_Expression_Paths
        .Else_Expression_Path_Access)
   is
   begin
      Self.Image.Append ("Else_Expression_Path");
   end Else_Expression_Path;

   ---------------
   -- Else_Path --
   ---------------

   overriding procedure Else_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Else_Paths.Else_Path_Access)
   is
   begin
      Self.Image.Append ("Else_Path");
   end Else_Path;

   ---------------------------
   -- Elsif_Expression_Path --
   ---------------------------

   overriding procedure Elsif_Expression_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Elsif_Expression_Paths
        .Elsif_Expression_Path_Access)
   is
   begin
      Self.Image.Append ("Elsif_Expression_Path");
   end Elsif_Expression_Path;

   ----------------
   -- Elsif_Path --
   ----------------

   overriding procedure Elsif_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Elsif_Paths.Elsif_Path_Access)
   is
   begin
      Self.Image.Append ("Elsif_Path");
   end Elsif_Path;

   ----------------
   -- Entry_Body --
   ----------------

   overriding procedure Entry_Body
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Entry_Bodies.Entry_Body_Access)
   is
   begin
      Self.Image.Append ("Entry_Body");
   end Entry_Body;

   -----------------------
   -- Entry_Declaration --
   -----------------------

   overriding procedure Entry_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Entry_Declarations
        .Entry_Declaration_Access)
   is
   begin
      Self.Image.Append ("Entry_Declaration");
   end Entry_Declaration;

   -------------------------------
   -- Entry_Index_Specification --
   -------------------------------

   overriding procedure Entry_Index_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Entry_Index_Specifications
        .Entry_Index_Specification_Access)
   is
   begin
      Self.Image.Append ("Entry_Index_Specification");
   end Entry_Index_Specification;

   ---------------------------------------
   -- Enumeration_Literal_Specification --
   ---------------------------------------

   overriding procedure Enumeration_Literal_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Enumeration_Literal_Specifications
        .Enumeration_Literal_Specification_Access)
   is
   begin
      Self.Image.Append ("Enumeration_Literal_Specification");
   end Enumeration_Literal_Specification;

   ---------------------------------
   -- Enumeration_Type_Definition --
   ---------------------------------

   overriding procedure Enumeration_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Enumeration_Type_Definitions
        .Enumeration_Type_Definition_Access)
   is
   begin
      Self.Image.Append ("Enumeration_Type_Definition");
   end Enumeration_Type_Definition;

   ---------------------------
   -- Exception_Declaration --
   ---------------------------

   overriding procedure Exception_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Exception_Declarations
        .Exception_Declaration_Access)
   is
   begin
      Self.Image.Append ("Exception_Declaration");
   end Exception_Declaration;

   -----------------------
   -- Exception_Handler --
   -----------------------

   overriding procedure Exception_Handler
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Exception_Handlers
        .Exception_Handler_Access)
   is
   begin
      Self.Image.Append ("Exception_Handler");
   end Exception_Handler;

   ------------------------------------
   -- Exception_Renaming_Declaration --
   ------------------------------------

   overriding procedure Exception_Renaming_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Exception_Renaming_Declarations
        .Exception_Renaming_Declaration_Access)
   is
   begin
      Self.Image.Append ("Exception_Renaming_Declaration");
   end Exception_Renaming_Declaration;

   --------------------
   -- Exit_Statement --
   --------------------

   overriding procedure Exit_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Exit_Statements.Exit_Statement_Access)
   is
   begin
      Self.Image.Append ("Exit_Statement");
   end Exit_Statement;

   --------------------------
   -- Explicit_Dereference --
   --------------------------

   overriding procedure Explicit_Dereference
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Explicit_Dereferences
        .Explicit_Dereference_Access)
   is
   begin
      Self.Image.Append ("Explicit_Dereference");
   end Explicit_Dereference;

   -------------------------------
   -- Extended_Return_Statement --
   -------------------------------

   overriding procedure Extended_Return_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Extended_Return_Statements
        .Extended_Return_Statement_Access)
   is
   begin
      Self.Image.Append ("Extended_Return_Statement");
   end Extended_Return_Statement;

   -------------------------
   -- Extension_Aggregate --
   -------------------------

   overriding procedure Extension_Aggregate
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Extension_Aggregates
        .Extension_Aggregate_Access)
   is
   begin
      Self.Image.Append ("Extension_Aggregate");
   end Extension_Aggregate;

   -------------------------------
   -- Floating_Point_Definition --
   -------------------------------

   overriding procedure Floating_Point_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Floating_Point_Definitions
        .Floating_Point_Definition_Access)
   is
   begin
      Self.Image.Append ("Floating_Point_Definition");
   end Floating_Point_Definition;

   ------------------------
   -- For_Loop_Statement --
   ------------------------

   overriding procedure For_Loop_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.For_Loop_Statements
        .For_Loop_Statement_Access)
   is
   begin
      Self.Image.Append ("For_Loop_Statement");
   end For_Loop_Statement;

   ------------------------------------------
   -- Formal_Access_To_Function_Definition --
   ------------------------------------------

   overriding procedure Formal_Access_To_Function_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Access_To_Function_Definitions
        .Formal_Access_To_Function_Definition_Access)
   is
   begin
      Self.Image.Append ("Formal_Access_To_Function_Definition");
   end Formal_Access_To_Function_Definition;

   ----------------------------------------
   -- Formal_Access_To_Object_Definition --
   ----------------------------------------

   overriding procedure Formal_Access_To_Object_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Access_To_Object_Definitions
        .Formal_Access_To_Object_Definition_Access)
   is
   begin
      Self.Image.Append ("Formal_Access_To_Object_Definition");
   end Formal_Access_To_Object_Definition;

   -------------------------------------------
   -- Formal_Access_To_Procedure_Definition --
   -------------------------------------------

   overriding procedure Formal_Access_To_Procedure_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Access_To_Procedure_Definitions
        .Formal_Access_To_Procedure_Definition_Access)
   is
   begin
      Self.Image.Append ("Formal_Access_To_Procedure_Definition");
   end Formal_Access_To_Procedure_Definition;

   -----------------------------------------
   -- Formal_Constrained_Array_Definition --
   -----------------------------------------

   overriding procedure Formal_Constrained_Array_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Constrained_Array_Definitions
        .Formal_Constrained_Array_Definition_Access)
   is
   begin
      Self.Image.Append ("Formal_Constrained_Array_Definition");
   end Formal_Constrained_Array_Definition;

   -------------------------------------------
   -- Formal_Decimal_Fixed_Point_Definition --
   -------------------------------------------

   overriding procedure Formal_Decimal_Fixed_Point_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Decimal_Fixed_Point_Definitions
        .Formal_Decimal_Fixed_Point_Definition_Access)
   is
   begin
      Self.Image.Append ("Formal_Decimal_Fixed_Point_Definition");
   end Formal_Decimal_Fixed_Point_Definition;

   ------------------------------------
   -- Formal_Derived_Type_Definition --
   ------------------------------------

   overriding procedure Formal_Derived_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Derived_Type_Definitions
        .Formal_Derived_Type_Definition_Access)
   is
   begin
      Self.Image.Append ("Formal_Derived_Type_Definition");
   end Formal_Derived_Type_Definition;

   -------------------------------------
   -- Formal_Discrete_Type_Definition --
   -------------------------------------

   overriding procedure Formal_Discrete_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Discrete_Type_Definitions
        .Formal_Discrete_Type_Definition_Access)
   is
   begin
      Self.Image.Append ("Formal_Discrete_Type_Definition");
   end Formal_Discrete_Type_Definition;

   --------------------------------------
   -- Formal_Floating_Point_Definition --
   --------------------------------------

   overriding procedure Formal_Floating_Point_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Floating_Point_Definitions
        .Formal_Floating_Point_Definition_Access)
   is
   begin
      Self.Image.Append ("Formal_Floating_Point_Definition");
   end Formal_Floating_Point_Definition;

   ---------------------------------
   -- Formal_Function_Declaration --
   ---------------------------------

   overriding procedure Formal_Function_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Function_Declarations
        .Formal_Function_Declaration_Access)
   is
   begin
      Self.Image.Append ("Formal_Function_Declaration");
   end Formal_Function_Declaration;

   ----------------------------------------
   -- Formal_Incomplete_Type_Declaration --
   ----------------------------------------

   overriding procedure Formal_Incomplete_Type_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Incomplete_Type_Declarations
        .Formal_Incomplete_Type_Declaration_Access)
   is
   begin
      Self.Image.Append ("Formal_Incomplete_Type_Declaration");
   end Formal_Incomplete_Type_Declaration;

   --------------------------------------
   -- Formal_Interface_Type_Definition --
   --------------------------------------

   overriding procedure Formal_Interface_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Interface_Type_Definitions
        .Formal_Interface_Type_Definition_Access)
   is
   begin
      Self.Image.Append ("Formal_Interface_Type_Definition");
   end Formal_Interface_Type_Definition;

   ------------------------------------
   -- Formal_Modular_Type_Definition --
   ------------------------------------

   overriding procedure Formal_Modular_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Modular_Type_Definitions
        .Formal_Modular_Type_Definition_Access)
   is
   begin
      Self.Image.Append ("Formal_Modular_Type_Definition");
   end Formal_Modular_Type_Definition;

   -------------------------------
   -- Formal_Object_Declaration --
   -------------------------------

   overriding procedure Formal_Object_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Object_Declarations
        .Formal_Object_Declaration_Access)
   is
   begin
      Self.Image.Append ("Formal_Object_Declaration");
   end Formal_Object_Declaration;

   --------------------------------------------
   -- Formal_Ordinary_Fixed_Point_Definition --
   --------------------------------------------

   overriding procedure Formal_Ordinary_Fixed_Point_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Ordinary_Fixed_Point_Definitions
        .Formal_Ordinary_Fixed_Point_Definition_Access)
   is
   begin
      Self.Image.Append ("Formal_Ordinary_Fixed_Point_Definition");
   end Formal_Ordinary_Fixed_Point_Definition;

   --------------------------------
   -- Formal_Package_Declaration --
   --------------------------------

   overriding procedure Formal_Package_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Package_Declarations
        .Formal_Package_Declaration_Access)
   is
   begin
      Self.Image.Append ("Formal_Package_Declaration");
   end Formal_Package_Declaration;

   ------------------------------------
   -- Formal_Private_Type_Definition --
   ------------------------------------

   overriding procedure Formal_Private_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Private_Type_Definitions
        .Formal_Private_Type_Definition_Access)
   is
   begin
      Self.Image.Append ("Formal_Private_Type_Definition");
   end Formal_Private_Type_Definition;

   ----------------------------------
   -- Formal_Procedure_Declaration --
   ----------------------------------

   overriding procedure Formal_Procedure_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Procedure_Declarations
        .Formal_Procedure_Declaration_Access)
   is
   begin
      Self.Image.Append ("Formal_Procedure_Declaration");
   end Formal_Procedure_Declaration;

   -------------------------------------------
   -- Formal_Signed_Integer_Type_Definition --
   -------------------------------------------

   overriding procedure Formal_Signed_Integer_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Signed_Integer_Type_Definitions
        .Formal_Signed_Integer_Type_Definition_Access)
   is
   begin
      Self.Image.Append ("Formal_Signed_Integer_Type_Definition");
   end Formal_Signed_Integer_Type_Definition;

   -----------------------------
   -- Formal_Type_Declaration --
   -----------------------------

   overriding procedure Formal_Type_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Type_Declarations
        .Formal_Type_Declaration_Access)
   is
   begin
      Self.Image.Append ("Formal_Type_Declaration");
   end Formal_Type_Declaration;

   -------------------------------------------
   -- Formal_Unconstrained_Array_Definition --
   -------------------------------------------

   overriding procedure Formal_Unconstrained_Array_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Formal_Unconstrained_Array_Definitions
        .Formal_Unconstrained_Array_Definition_Access)
   is
   begin
      Self.Image.Append ("Formal_Unconstrained_Array_Definition");
   end Formal_Unconstrained_Array_Definition;

   ---------------------------
   -- Full_Type_Declaration --
   ---------------------------

   overriding procedure Full_Type_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Full_Type_Declarations
        .Full_Type_Declaration_Access)
   is
   begin
      Self.Image.Append ("Full_Type_Declaration");
   end Full_Type_Declaration;

   -------------------
   -- Function_Body --
   -------------------

   overriding procedure Function_Body
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Function_Bodies.Function_Body_Access)
   is
   begin
      Self.Image.Append ("Function_Body");
   end Function_Body;

   -------------------
   -- Function_Call --
   -------------------

   overriding procedure Function_Call
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Function_Calls.Function_Call_Access)
   is
   begin
      Self.Image.Append ("Function_Call");
   end Function_Call;

   --------------------------
   -- Function_Declaration --
   --------------------------

   overriding procedure Function_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Function_Declarations
        .Function_Declaration_Access)
   is
   begin
      Self.Image.Append ("Function_Declaration");
   end Function_Declaration;

   ----------------------------
   -- Function_Instantiation --
   ----------------------------

   overriding procedure Function_Instantiation
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Function_Instantiations
        .Function_Instantiation_Access)
   is
   begin
      Self.Image.Append ("Function_Instantiation");
   end Function_Instantiation;

   ----------------------------------------
   -- Generalized_Iterator_Specification --
   ----------------------------------------

   overriding procedure Generalized_Iterator_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Generalized_Iterator_Specifications
        .Generalized_Iterator_Specification_Access)
   is
   begin
      Self.Image.Append ("Generalized_Iterator_Specification");
   end Generalized_Iterator_Specification;

   -------------------------
   -- Generic_Association --
   -------------------------

   overriding procedure Generic_Association
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Generic_Associations
        .Generic_Association_Access)
   is
   begin
      Self.Image.Append ("Generic_Association");
   end Generic_Association;

   ----------------------------------
   -- Generic_Function_Declaration --
   ----------------------------------

   overriding procedure Generic_Function_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Generic_Function_Declarations
        .Generic_Function_Declaration_Access)
   is
   begin
      Self.Image.Append ("Generic_Function_Declaration");
   end Generic_Function_Declaration;

   -------------------------------
   -- Generic_Function_Renaming --
   -------------------------------

   overriding procedure Generic_Function_Renaming
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Generic_Function_Renamings
        .Generic_Function_Renaming_Access)
   is
   begin
      Self.Image.Append ("Generic_Function_Renaming");
   end Generic_Function_Renaming;

   ---------------------------------
   -- Generic_Package_Declaration --
   ---------------------------------

   overriding procedure Generic_Package_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Generic_Package_Declarations
        .Generic_Package_Declaration_Access)
   is
   begin
      Self.Image.Append ("Generic_Package_Declaration");
   end Generic_Package_Declaration;

   ------------------------------
   -- Generic_Package_Renaming --
   ------------------------------

   overriding procedure Generic_Package_Renaming
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Generic_Package_Renamings
        .Generic_Package_Renaming_Access)
   is
   begin
      Self.Image.Append ("Generic_Package_Renaming");
   end Generic_Package_Renaming;

   -----------------------------------
   -- Generic_Procedure_Declaration --
   -----------------------------------

   overriding procedure Generic_Procedure_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Generic_Procedure_Declarations
        .Generic_Procedure_Declaration_Access)
   is
   begin
      Self.Image.Append ("Generic_Procedure_Declaration");
   end Generic_Procedure_Declaration;

   --------------------------------
   -- Generic_Procedure_Renaming --
   --------------------------------

   overriding procedure Generic_Procedure_Renaming
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Generic_Procedure_Renamings
        .Generic_Procedure_Renaming_Access)
   is
   begin
      Self.Image.Append ("Generic_Procedure_Renaming");
   end Generic_Procedure_Renaming;

   --------------------
   -- Goto_Statement --
   --------------------

   overriding procedure Goto_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Goto_Statements.Goto_Statement_Access)
   is
   begin
      Self.Image.Append ("Goto_Statement");
   end Goto_Statement;

   ----------------
   -- Identifier --
   ----------------

   overriding procedure Identifier
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Identifiers.Identifier_Access)
   is
   begin
      Self.Image.Append ("Identifier");
   end Identifier;

   -------------------
   -- If_Expression --
   -------------------

   overriding procedure If_Expression
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.If_Expressions.If_Expression_Access)
   is
   begin
      Self.Image.Append ("If_Expression");
   end If_Expression;

   ------------------------
   -- If_Expression_Path --
   ------------------------

   overriding procedure If_Expression_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.If_Expression_Paths
        .If_Expression_Path_Access)
   is
   begin
      Self.Image.Append ("If_Expression_Path");
   end If_Expression_Path;

   -------------
   -- If_Path --
   -------------

   overriding procedure If_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.If_Paths.If_Path_Access)
   is
   begin
      Self.Image.Append ("If_Path");
   end If_Path;

   ------------------
   -- If_Statement --
   ------------------

   overriding procedure If_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.If_Statements.If_Statement_Access)
   is
   begin
      Self.Image.Append ("If_Statement");
   end If_Statement;

   ---------------------------------
   -- Incomplete_Type_Declaration --
   ---------------------------------

   overriding procedure Incomplete_Type_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Incomplete_Type_Declarations
        .Incomplete_Type_Declaration_Access)
   is
   begin
      Self.Image.Append ("Incomplete_Type_Declaration");
   end Incomplete_Type_Declaration;

   --------------------------------
   -- Incomplete_Type_Definition --
   --------------------------------

   overriding procedure Incomplete_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Incomplete_Type_Definitions
        .Incomplete_Type_Definition_Access)
   is
   begin
      Self.Image.Append ("Incomplete_Type_Definition");
   end Incomplete_Type_Definition;

   -------------------------------
   -- Interface_Type_Definition --
   -------------------------------

   overriding procedure Interface_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Interface_Type_Definitions
        .Interface_Type_Definition_Access)
   is
   begin
      Self.Image.Append ("Interface_Type_Definition");
   end Interface_Type_Definition;

   -----------------------------
   -- Known_Discriminant_Part --
   -----------------------------

   overriding procedure Known_Discriminant_Part
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Known_Discriminant_Parts
        .Known_Discriminant_Part_Access)
   is
   begin
      Self.Image.Append ("Known_Discriminant_Part");
   end Known_Discriminant_Part;

   ---------------------
   -- Label_Decorator --
   ---------------------

   overriding procedure Label_Decorator
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Label_Decorators.Label_Decorator_Access)
   is
   begin
      Self.Image.Append ("Label_Decorator");
   end Label_Decorator;

   ----------------------------------
   -- Loop_Parameter_Specification --
   ----------------------------------

   overriding procedure Loop_Parameter_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Loop_Parameter_Specifications
        .Loop_Parameter_Specification_Access)
   is
   begin
      Self.Image.Append ("Loop_Parameter_Specification");
   end Loop_Parameter_Specification;

   --------------------
   -- Loop_Statement --
   --------------------

   overriding procedure Loop_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Loop_Statements.Loop_Statement_Access)
   is
   begin
      Self.Image.Append ("Loop_Statement");
   end Loop_Statement;

   ---------------------
   -- Membership_Test --
   ---------------------

   overriding procedure Membership_Test
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Membership_Tests.Membership_Test_Access)
   is
   begin
      Self.Image.Append ("Membership_Test");
   end Membership_Test;

   -----------------------------
   -- Modular_Type_Definition --
   -----------------------------

   overriding procedure Modular_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Modular_Type_Definitions
        .Modular_Type_Definition_Access)
   is
   begin
      Self.Image.Append ("Modular_Type_Definition");
   end Modular_Type_Definition;

   --------------------
   -- Null_Component --
   --------------------

   overriding procedure Null_Component
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Null_Components.Null_Component_Access)
   is
   begin
      Self.Image.Append ("Null_Component");
   end Null_Component;

   ------------------
   -- Null_Literal --
   ------------------

   overriding procedure Null_Literal
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Null_Literals.Null_Literal_Access)
   is
   begin
      Self.Image.Append ("Null_Literal");
   end Null_Literal;

   ----------------------------
   -- Null_Record_Definition --
   ----------------------------

   overriding procedure Null_Record_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Null_Record_Definitions
        .Null_Record_Definition_Access)
   is
   begin
      Self.Image.Append ("Null_Record_Definition");
   end Null_Record_Definition;

   --------------------
   -- Null_Statement --
   --------------------

   overriding procedure Null_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Null_Statements.Null_Statement_Access)
   is
   begin
      Self.Image.Append ("Null_Statement");
   end Null_Statement;

   ------------------------
   -- Number_Declaration --
   ------------------------

   overriding procedure Number_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Number_Declarations
        .Number_Declaration_Access)
   is
   begin
      Self.Image.Append ("Number_Declaration");
   end Number_Declaration;

   ---------------------
   -- Numeric_Literal --
   ---------------------

   overriding procedure Numeric_Literal
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Numeric_Literals.Numeric_Literal_Access)
   is
   begin
      Self.Image.Append ("Numeric_Literal");
   end Numeric_Literal;

   ------------------------
   -- Object_Declaration --
   ------------------------

   overriding procedure Object_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Object_Declarations
        .Object_Declaration_Access)
   is
   begin
      Self.Image.Append ("Object_Declaration");
   end Object_Declaration;

   ---------------------------------
   -- Object_Renaming_Declaration --
   ---------------------------------

   overriding procedure Object_Renaming_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Object_Renaming_Declarations
        .Object_Renaming_Declaration_Access)
   is
   begin
      Self.Image.Append ("Object_Renaming_Declaration");
   end Object_Renaming_Declaration;

   ---------------------
   -- Operator_Symbol --
   ---------------------

   overriding procedure Operator_Symbol
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Operator_Symbols.Operator_Symbol_Access)
   is
   begin
      Self.Image.Append ("Operator_Symbol");
   end Operator_Symbol;

   -------------------------------------
   -- Ordinary_Fixed_Point_Definition --
   -------------------------------------

   overriding procedure Ordinary_Fixed_Point_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Ordinary_Fixed_Point_Definitions
        .Ordinary_Fixed_Point_Definition_Access)
   is
   begin
      Self.Image.Append ("Ordinary_Fixed_Point_Definition");
   end Ordinary_Fixed_Point_Definition;

   -------------------
   -- Others_Choice --
   -------------------

   overriding procedure Others_Choice
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Others_Choices.Others_Choice_Access)
   is
   begin
      Self.Image.Append ("Others_Choice");
   end Others_Choice;

   ------------------
   -- Package_Body --
   ------------------

   overriding procedure Package_Body
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Package_Bodies.Package_Body_Access)
   is
   begin
      Self.Image.Append ("Package_Body");
   end Package_Body;

   -----------------------
   -- Package_Body_Stub --
   -----------------------

   overriding procedure Package_Body_Stub
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Package_Body_Stubs
        .Package_Body_Stub_Access)
   is
   begin
      Self.Image.Append ("Package_Body_Stub");
   end Package_Body_Stub;

   -------------------------
   -- Package_Declaration --
   -------------------------

   overriding procedure Package_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Package_Declarations
        .Package_Declaration_Access)
   is
   begin
      Self.Image.Append ("Package_Declaration");
   end Package_Declaration;

   ---------------------------
   -- Package_Instantiation --
   ---------------------------

   overriding procedure Package_Instantiation
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Package_Instantiations
        .Package_Instantiation_Access)
   is
   begin
      Self.Image.Append ("Package_Instantiation");
   end Package_Instantiation;

   ----------------------------------
   -- Package_Renaming_Declaration --
   ----------------------------------

   overriding procedure Package_Renaming_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Package_Renaming_Declarations
        .Package_Renaming_Declaration_Access)
   is
   begin
      Self.Image.Append ("Package_Renaming_Declaration");
   end Package_Renaming_Declaration;

   ---------------------------
   -- Parameter_Association --
   ---------------------------

   overriding procedure Parameter_Association
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Parameter_Associations
        .Parameter_Association_Access)
   is
   begin
      Self.Image.Append ("Parameter_Association");
   end Parameter_Association;

   -----------------------------
   -- Parameter_Specification --
   -----------------------------

   overriding procedure Parameter_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Parameter_Specifications
        .Parameter_Specification_Access)
   is
   begin
      Self.Image.Append ("Parameter_Specification");
   end Parameter_Specification;

   ------------------------------
   -- Parenthesized_Expression --
   ------------------------------

   overriding procedure Parenthesized_Expression
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Parenthesized_Expressions
        .Parenthesized_Expression_Access)
   is
   begin
      Self.Image.Append ("Parenthesized_Expression");
   end Parenthesized_Expression;

   ---------------------------------
   -- Pragma_Argument_Association --
   ---------------------------------

   overriding procedure Pragma_Argument_Association
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Pragma_Argument_Associations
        .Pragma_Argument_Association_Access)
   is
   begin
      Self.Image.Append ("Pragma_Argument_Association");
   end Pragma_Argument_Association;

   -----------------
   -- Pragma_Node --
   -----------------

   overriding procedure Pragma_Node
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Pragma_Nodes.Pragma_Node_Access)
   is
   begin
      Self.Image.Append ("Pragma_Node");
   end Pragma_Node;

   -----------------------------------
   -- Private_Extension_Declaration --
   -----------------------------------

   overriding procedure Private_Extension_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Private_Extension_Declarations
        .Private_Extension_Declaration_Access)
   is
   begin
      Self.Image.Append ("Private_Extension_Declaration");
   end Private_Extension_Declaration;

   ----------------------------------
   -- Private_Extension_Definition --
   ----------------------------------

   overriding procedure Private_Extension_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Private_Extension_Definitions
        .Private_Extension_Definition_Access)
   is
   begin
      Self.Image.Append ("Private_Extension_Definition");
   end Private_Extension_Definition;

   ------------------------------
   -- Private_Type_Declaration --
   ------------------------------

   overriding procedure Private_Type_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Private_Type_Declarations
        .Private_Type_Declaration_Access)
   is
   begin
      Self.Image.Append ("Private_Type_Declaration");
   end Private_Type_Declaration;

   -----------------------------
   -- Private_Type_Definition --
   -----------------------------

   overriding procedure Private_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Private_Type_Definitions
        .Private_Type_Definition_Access)
   is
   begin
      Self.Image.Append ("Private_Type_Definition");
   end Private_Type_Definition;

   --------------------
   -- Procedure_Body --
   --------------------

   overriding procedure Procedure_Body
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Procedure_Bodies.Procedure_Body_Access)
   is
   begin
      Self.Image.Append ("Procedure_Body");
   end Procedure_Body;

   ------------------------------
   -- Procedure_Call_Statement --
   ------------------------------

   overriding procedure Procedure_Call_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Procedure_Call_Statements
        .Procedure_Call_Statement_Access)
   is
   begin
      Self.Image.Append ("Procedure_Call_Statement");
   end Procedure_Call_Statement;

   ---------------------------
   -- Procedure_Declaration --
   ---------------------------

   overriding procedure Procedure_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Procedure_Declarations
        .Procedure_Declaration_Access)
   is
   begin
      Self.Image.Append ("Procedure_Declaration");
   end Procedure_Declaration;

   -----------------------------
   -- Procedure_Instantiation --
   -----------------------------

   overriding procedure Procedure_Instantiation
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Procedure_Instantiations
        .Procedure_Instantiation_Access)
   is
   begin
      Self.Image.Append ("Procedure_Instantiation");
   end Procedure_Instantiation;

   --------------------
   -- Protected_Body --
   --------------------

   overriding procedure Protected_Body
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Protected_Bodies.Protected_Body_Access)
   is
   begin
      Self.Image.Append ("Protected_Body");
   end Protected_Body;

   -------------------------
   -- Protected_Body_Stub --
   -------------------------

   overriding procedure Protected_Body_Stub
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Protected_Body_Stubs
        .Protected_Body_Stub_Access)
   is
   begin
      Self.Image.Append ("Protected_Body_Stub");
   end Protected_Body_Stub;

   --------------------------
   -- Protected_Definition --
   --------------------------

   overriding procedure Protected_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Protected_Definitions
        .Protected_Definition_Access)
   is
   begin
      Self.Image.Append ("Protected_Definition");
   end Protected_Definition;

   --------------------------------
   -- Protected_Type_Declaration --
   --------------------------------

   overriding procedure Protected_Type_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Protected_Type_Declarations
        .Protected_Type_Declaration_Access)
   is
   begin
      Self.Image.Append ("Protected_Type_Declaration");
   end Protected_Type_Declaration;

   --------------------------
   -- Qualified_Expression --
   --------------------------

   overriding procedure Qualified_Expression
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Qualified_Expressions
        .Qualified_Expression_Access)
   is
   begin
      Self.Image.Append ("Qualified_Expression");
   end Qualified_Expression;

   ---------------------------
   -- Quantified_Expression --
   ---------------------------

   overriding procedure Quantified_Expression
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Quantified_Expressions
        .Quantified_Expression_Access)
   is
   begin
      Self.Image.Append ("Quantified_Expression");
   end Quantified_Expression;

   ---------------------
   -- Raise_Statement --
   ---------------------

   overriding procedure Raise_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Raise_Statements.Raise_Statement_Access)
   is
   begin
      Self.Image.Append ("Raise_Statement");
   end Raise_Statement;

   -------------------------------
   -- Range_Attribute_Reference --
   -------------------------------

   overriding procedure Range_Attribute_Reference
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Range_Attribute_References
        .Range_Attribute_Reference_Access)
   is
   begin
      Self.Image.Append ("Range_Attribute_Reference");
   end Range_Attribute_Reference;

   ----------------------------------
   -- Range_Attribute_Reference_Dr --
   ----------------------------------

   overriding procedure Range_Attribute_Reference_Dr
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Range_Attribute_Reference_Drs
        .Range_Attribute_Reference_Dr_Access)
   is
   begin
      Self.Image.Append ("Range_Attribute_Reference_Dr");
   end Range_Attribute_Reference_Dr;

   ----------------------
   -- Record_Aggregate --
   ----------------------

   overriding procedure Record_Aggregate
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Record_Aggregates.Record_Aggregate_Access)
   is
   begin
      Self.Image.Append ("Record_Aggregate");
   end Record_Aggregate;

   -----------------------
   -- Record_Definition --
   -----------------------

   overriding procedure Record_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Record_Definitions
        .Record_Definition_Access)
   is
   begin
      Self.Image.Append ("Record_Definition");
   end Record_Definition;

   ----------------------------------
   -- Record_Representation_Clause --
   ----------------------------------

   overriding procedure Record_Representation_Clause
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Record_Representation_Clauses
        .Record_Representation_Clause_Access)
   is
   begin
      Self.Image.Append ("Record_Representation_Clause");
   end Record_Representation_Clause;

   ----------------------------
   -- Record_Type_Definition --
   ----------------------------

   overriding procedure Record_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Record_Type_Definitions
        .Record_Type_Definition_Access)
   is
   begin
      Self.Image.Append ("Record_Type_Definition");
   end Record_Type_Definition;

   -----------------------
   -- Requeue_Statement --
   -----------------------

   overriding procedure Requeue_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Requeue_Statements
        .Requeue_Statement_Access)
   is
   begin
      Self.Image.Append ("Requeue_Statement");
   end Requeue_Statement;

   ---------------------------------
   -- Return_Object_Specification --
   ---------------------------------

   overriding procedure Return_Object_Specification
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Return_Object_Specifications
        .Return_Object_Specification_Access)
   is
   begin
      Self.Image.Append ("Return_Object_Specification");
   end Return_Object_Specification;

   --------------------------
   -- Root_Type_Definition --
   --------------------------

   overriding procedure Root_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Root_Type_Definitions
        .Root_Type_Definition_Access)
   is
   begin
      Self.Image.Append ("Root_Type_Definition");
   end Root_Type_Definition;

   -------------------------------
   -- Scalar_Subtype_Indication --
   -------------------------------

   overriding procedure Scalar_Subtype_Indication
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Scalar_Subtype_Indications
        .Scalar_Subtype_Indication_Access)
   is
   begin
      Self.Image.Append ("Scalar_Subtype_Indication");
   end Scalar_Subtype_Indication;

   --------------------
   -- Select_Or_Path --
   --------------------

   overriding procedure Select_Or_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Select_Or_Paths.Select_Or_Path_Access)
   is
   begin
      Self.Image.Append ("Select_Or_Path");
   end Select_Or_Path;

   ------------------------
   -- Selected_Component --
   ------------------------

   overriding procedure Selected_Component
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Selected_Components
        .Selected_Component_Access)
   is
   begin
      Self.Image.Append ("Selected_Component");
   end Selected_Component;

   -------------------------
   -- Selected_Identifier --
   -------------------------

   overriding procedure Selected_Identifier
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Selected_Identifiers
        .Selected_Identifier_Access)
   is
   begin
      Self.Image.Append ("Selected_Identifier");
   end Selected_Identifier;

   ----------------------
   -- Selective_Accept --
   ----------------------

   overriding procedure Selective_Accept
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Selective_Accepts.Selective_Accept_Access)
   is
   begin
      Self.Image.Append ("Selective_Accept");
   end Selective_Accept;

   -------------------
   -- Short_Circuit --
   -------------------

   overriding procedure Short_Circuit
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Short_Circuits.Short_Circuit_Access)
   is
   begin
      Self.Image.Append ("Short_Circuit");
   end Short_Circuit;

   ------------------------------------
   -- Signed_Integer_Type_Definition --
   ------------------------------------

   overriding procedure Signed_Integer_Type_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Signed_Integer_Type_Definitions
        .Signed_Integer_Type_Definition_Access)
   is
   begin
      Self.Image.Append ("Signed_Integer_Type_Definition");
   end Signed_Integer_Type_Definition;

   -----------------------------
   -- Simple_Expression_Range --
   -----------------------------

   overriding procedure Simple_Expression_Range
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Simple_Expression_Ranges
        .Simple_Expression_Range_Access)
   is
   begin
      Self.Image.Append ("Simple_Expression_Range");
   end Simple_Expression_Range;

   --------------------------------
   -- Simple_Expression_Range_Dr --
   --------------------------------

   overriding procedure Simple_Expression_Range_Dr
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Simple_Expression_Range_Drs
        .Simple_Expression_Range_Dr_Access)
   is
   begin
      Self.Image.Append ("Simple_Expression_Range_Dr");
   end Simple_Expression_Range_Dr;

   -----------------------------
   -- Simple_Return_Statement --
   -----------------------------

   overriding procedure Simple_Return_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Simple_Return_Statements
        .Simple_Return_Statement_Access)
   is
   begin
      Self.Image.Append ("Simple_Return_Statement");
   end Simple_Return_Statement;

   ----------------------------------
   -- Single_Protected_Declaration --
   ----------------------------------

   overriding procedure Single_Protected_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Single_Protected_Declarations
        .Single_Protected_Declaration_Access)
   is
   begin
      Self.Image.Append ("Single_Protected_Declaration");
   end Single_Protected_Declaration;

   -----------------------------
   -- Single_Task_Declaration --
   -----------------------------

   overriding procedure Single_Task_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Single_Task_Declarations
        .Single_Task_Declaration_Access)
   is
   begin
      Self.Image.Append ("Single_Task_Declaration");
   end Single_Task_Declaration;

   --------------------
   -- String_Literal --
   --------------------

   overriding procedure String_Literal
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.String_Literals.String_Literal_Access)
   is
   begin
      Self.Image.Append ("String_Literal");
   end String_Literal;

   -------------------------
   -- Subtype_Declaration --
   -------------------------

   overriding procedure Subtype_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Subtype_Declarations
        .Subtype_Declaration_Access)
   is
   begin
      Self.Image.Append ("Subtype_Declaration");
   end Subtype_Declaration;

   -------------
   -- Subunit --
   -------------

   overriding procedure Subunit
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Subunits.Subunit_Access)
   is
   begin
      Self.Image.Append ("Subunit");
   end Subunit;

   ---------------
   -- Task_Body --
   ---------------

   overriding procedure Task_Body
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Task_Bodies.Task_Body_Access)
   is
   begin
      Self.Image.Append ("Task_Body");
   end Task_Body;

   --------------------
   -- Task_Body_Stub --
   --------------------

   overriding procedure Task_Body_Stub
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Task_Body_Stubs.Task_Body_Stub_Access)
   is
   begin
      Self.Image.Append ("Task_Body_Stub");
   end Task_Body_Stub;

   ---------------------
   -- Task_Definition --
   ---------------------

   overriding procedure Task_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Task_Definitions.Task_Definition_Access)
   is
   begin
      Self.Image.Append ("Task_Definition");
   end Task_Definition;

   ---------------------------
   -- Task_Type_Declaration --
   ---------------------------

   overriding procedure Task_Type_Declaration
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Task_Type_Declarations
        .Task_Type_Declaration_Access)
   is
   begin
      Self.Image.Append ("Task_Type_Declaration");
   end Task_Type_Declaration;

   -------------------------------------
   -- Terminate_Alternative_Statement --
   -------------------------------------

   overriding procedure Terminate_Alternative_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Terminate_Alternative_Statements
        .Terminate_Alternative_Statement_Access)
   is
   begin
      Self.Image.Append ("Terminate_Alternative_Statement");
   end Terminate_Alternative_Statement;

   ---------------------
   -- Then_Abort_Path --
   ---------------------

   overriding procedure Then_Abort_Path
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Then_Abort_Paths.Then_Abort_Path_Access)
   is
   begin
      Self.Image.Append ("Then_Abort_Path");
   end Then_Abort_Path;

   ------------------------------------
   -- Unconstrained_Array_Definition --
   ------------------------------------

   overriding procedure Unconstrained_Array_Definition
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Unconstrained_Array_Definitions
        .Unconstrained_Array_Definition_Access)
   is
   begin
      Self.Image.Append ("Unconstrained_Array_Definition");
   end Unconstrained_Array_Definition;

   -------------------------------
   -- Unknown_Discriminant_Part --
   -------------------------------

   overriding procedure Unknown_Discriminant_Part
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Unknown_Discriminant_Parts
        .Unknown_Discriminant_Part_Access)
   is
   begin
      Self.Image.Append ("Unknown_Discriminant_Part");
   end Unknown_Discriminant_Part;

   ------------------------
   -- Use_Package_Clause --
   ------------------------

   overriding procedure Use_Package_Clause
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Use_Package_Clauses
        .Use_Package_Clause_Access)
   is
   begin
      Self.Image.Append ("Use_Package_Clause");
   end Use_Package_Clause;

   ---------------------
   -- Use_Type_Clause --
   ---------------------

   overriding procedure Use_Type_Clause
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Use_Type_Clauses.Use_Type_Clause_Access)
   is
   begin
      Self.Image.Append ("Use_Type_Clause");
   end Use_Type_Clause;

   -------------
   -- Variant --
   -------------

   overriding procedure Variant
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Variants.Variant_Access)
   is
   begin
      Self.Image.Append ("Variant");
   end Variant;

   ------------------
   -- Variant_Part --
   ------------------

   overriding procedure Variant_Part
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.Variant_Parts.Variant_Part_Access)
   is
   begin
      Self.Image.Append ("Variant_Part");
   end Variant_Part;

   --------------------------
   -- While_Loop_Statement --
   --------------------------

   overriding procedure While_Loop_Statement
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.While_Loop_Statements
        .While_Loop_Statement_Access)
   is
   begin
      Self.Image.Append ("While_Loop_Statement");
   end While_Loop_Statement;

   -----------------
   -- With_Clause --
   -----------------

   overriding procedure With_Clause
     (Self : in out Element_Printer;
      Node : not null Gela.Elements.With_Clauses.With_Clause_Access)
   is
   begin
      Self.Image.Append ("With_Clause");
   end With_Clause;

end Element_Printers;
