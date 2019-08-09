--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes is

   -----------------------
   -- Enclosing_Element --
   -----------------------

   overriding function Enclosing_Element
     (Self : Node) return Program.Elements.Element_Access is
   begin
      return Self.Enclosing_Element;
   end Enclosing_Element;

   ------------------------
   -- Is_Abort_Statement --
   ------------------------

   overriding function Is_Abort_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Abort_Statement;

   -------------------------
   -- Is_Accept_Statement --
   -------------------------

   overriding function Is_Accept_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Accept_Statement;

   --------------------
   -- Is_Access_Type --
   --------------------

   overriding function Is_Access_Type (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Access_Type;

   ------------------
   -- Is_Allocator --
   ------------------

   overriding function Is_Allocator (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Allocator;

   ------------------------------------
   -- Is_Anonymous_Access_Definition --
   ------------------------------------

   overriding function Is_Anonymous_Access_Definition
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Anonymous_Access_Definition;

   -----------------------------------
   -- Is_Anonymous_Access_To_Object --
   -----------------------------------

   overriding function Is_Anonymous_Access_To_Object
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Anonymous_Access_To_Object;

   --------------------------------------
   -- Is_Anonymous_Access_To_Procedure --
   --------------------------------------

   overriding function Is_Anonymous_Access_To_Procedure
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Anonymous_Access_To_Procedure;

   -------------------------------------
   -- Is_Anonymous_Access_To_Function --
   -------------------------------------

   overriding function Is_Anonymous_Access_To_Function
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Anonymous_Access_To_Function;

   ------------------------
   -- Is_Array_Aggregate --
   ------------------------

   overriding function Is_Array_Aggregate (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Array_Aggregate;

   ------------------------------------
   -- Is_Array_Component_Association --
   ------------------------------------

   overriding function Is_Array_Component_Association
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Array_Component_Association;

   -----------------------------
   -- Is_Aspect_Specification --
   -----------------------------

   overriding function Is_Aspect_Specification (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Aspect_Specification;

   -----------------------------
   -- Is_Assignment_Statement --
   -----------------------------

   overriding function Is_Assignment_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Assignment_Statement;

   --------------------
   -- Is_Association --
   --------------------

   overriding function Is_Association (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Association;

   ------------------
   -- Is_At_Clause --
   ------------------

   overriding function Is_At_Clause (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_At_Clause;

   ------------------------------------
   -- Is_Attribute_Definition_Clause --
   ------------------------------------

   overriding function Is_Attribute_Definition_Clause
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Attribute_Definition_Clause;

   ----------------------------
   -- Is_Attribute_Reference --
   ----------------------------

   overriding function Is_Attribute_Reference (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Attribute_Reference;

   ------------------------
   -- Is_Block_Statement --
   ------------------------

   overriding function Is_Block_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Block_Statement;

   -----------------------
   -- Is_Call_Statement --
   -----------------------

   overriding function Is_Call_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Call_Statement;

   ------------------------
   -- Is_Case_Expression --
   ------------------------

   overriding function Is_Case_Expression (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Case_Expression;

   -----------------------------
   -- Is_Case_Expression_Path --
   -----------------------------

   overriding function Is_Case_Expression_Path (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Case_Expression_Path;

   ------------------
   -- Is_Case_Path --
   ------------------

   overriding function Is_Case_Path (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Case_Path;

   -----------------------
   -- Is_Case_Statement --
   -----------------------

   overriding function Is_Case_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Case_Statement;

   --------------------------
   -- Is_Character_Literal --
   --------------------------

   overriding function Is_Character_Literal (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Character_Literal;

   ---------------------------------------
   -- Is_Choice_Parameter_Specification --
   ---------------------------------------

   overriding function Is_Choice_Parameter_Specification
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Choice_Parameter_Specification;

   ---------------
   -- Is_Clause --
   ---------------

   overriding function Is_Clause (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Clause;

   -----------------------
   -- Is_Code_Statement --
   -----------------------

   overriding function Is_Code_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Code_Statement;

   -------------------------
   -- Is_Component_Clause --
   -------------------------

   overriding function Is_Component_Clause (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Component_Clause;

   ------------------------------
   -- Is_Component_Declaration --
   ------------------------------

   overriding function Is_Component_Declaration (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Component_Declaration;

   -----------------------------
   -- Is_Component_Definition --
   -----------------------------

   overriding function Is_Component_Definition (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Component_Definition;

   -------------------------------
   -- Is_Constrained_Array_Type --
   -------------------------------

   overriding function Is_Constrained_Array_Type (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Constrained_Array_Type;

   -------------------
   -- Is_Constraint --
   -------------------

   overriding function Is_Constraint (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Constraint;

   ---------------------------------
   -- Is_Decimal_Fixed_Point_Type --
   ---------------------------------

   overriding function Is_Decimal_Fixed_Point_Type (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Decimal_Fixed_Point_Type;

   --------------------
   -- Is_Declaration --
   --------------------

   overriding function Is_Declaration (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Declaration;

   -----------------------------------
   -- Is_Defining_Character_Literal --
   -----------------------------------

   overriding function Is_Defining_Character_Literal
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Defining_Character_Literal;

   -------------------------------
   -- Is_Defining_Expanded_Name --
   -------------------------------

   overriding function Is_Defining_Expanded_Name (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Defining_Expanded_Name;

   ----------------------------
   -- Is_Defining_Identifier --
   ----------------------------

   overriding function Is_Defining_Identifier (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Defining_Identifier;

   ----------------------
   -- Is_Defining_Name --
   ----------------------

   overriding function Is_Defining_Name (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Defining_Name;

   ---------------------------------
   -- Is_Defining_Operator_Symbol --
   ---------------------------------

   overriding function Is_Defining_Operator_Symbol (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Defining_Operator_Symbol;

   -------------------
   -- Is_Definition --
   -------------------

   overriding function Is_Definition (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Definition;

   ------------------------
   -- Is_Delay_Statement --
   ------------------------

   overriding function Is_Delay_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Delay_Statement;

   -------------------------
   -- Is_Delta_Constraint --
   -------------------------

   overriding function Is_Delta_Constraint (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Delta_Constraint;

   ---------------------------------
   -- Is_Derived_Record_Extension --
   ---------------------------------

   overriding function Is_Derived_Record_Extension (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Derived_Record_Extension;

   ---------------------
   -- Is_Derived_Type --
   ---------------------

   overriding function Is_Derived_Type (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Derived_Type;

   --------------------------
   -- Is_Digits_Constraint --
   --------------------------

   overriding function Is_Digits_Constraint (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Digits_Constraint;

   -----------------------
   -- Is_Discrete_Range --
   -----------------------

   overriding function Is_Discrete_Range (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Discrete_Range;

   -------------------------------------------
   -- Is_Discrete_Range_Attribute_Reference --
   -------------------------------------------

   overriding function Is_Discrete_Range_Attribute_Reference
     (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Discrete_Range_Attribute_Reference;

   -----------------------------------------
   -- Is_Discrete_Simple_Expression_Range --
   -----------------------------------------

   overriding function Is_Discrete_Simple_Expression_Range
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Discrete_Simple_Expression_Range;

   ------------------------------------
   -- Is_Discrete_Subtype_Indication --
   ------------------------------------

   overriding function Is_Discrete_Subtype_Indication
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Discrete_Subtype_Indication;

   ---------------------------------
   -- Is_Discriminant_Association --
   ---------------------------------

   overriding function Is_Discriminant_Association (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Discriminant_Association;

   --------------------------------
   -- Is_Discriminant_Constraint --
   --------------------------------

   overriding function Is_Discriminant_Constraint (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Discriminant_Constraint;

   -----------------------------------
   -- Is_Discriminant_Specification --
   -----------------------------------

   overriding function Is_Discriminant_Specification
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Discriminant_Specification;

   ---------------------------------------
   -- Is_Element_Iterator_Specification --
   ---------------------------------------

   overriding function Is_Element_Iterator_Specification
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Element_Iterator_Specification;

   ------------------------------
   -- Is_Elsif_Expression_Path --
   ------------------------------

   overriding function Is_Elsif_Expression_Path (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Elsif_Expression_Path;

   -------------------
   -- Is_Elsif_Path --
   -------------------

   overriding function Is_Elsif_Path (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Elsif_Path;

   -------------------------------
   -- Is_Entry_Body_Declaration --
   -------------------------------

   overriding function Is_Entry_Body_Declaration (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Entry_Body_Declaration;

   --------------------------
   -- Is_Entry_Declaration --
   --------------------------

   overriding function Is_Entry_Declaration (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Entry_Declaration;

   ----------------------------------
   -- Is_Entry_Index_Specification --
   ----------------------------------

   overriding function Is_Entry_Index_Specification
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Entry_Index_Specification;

   ------------------------------------------
   -- Is_Enumeration_Literal_Specification --
   ------------------------------------------

   overriding function Is_Enumeration_Literal_Specification
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Enumeration_Literal_Specification;

   ------------------------------------------
   -- Is_Enumeration_Representation_Clause --
   ------------------------------------------

   overriding function Is_Enumeration_Representation_Clause
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Enumeration_Representation_Clause;

   -------------------------
   -- Is_Enumeration_Type --
   -------------------------

   overriding function Is_Enumeration_Type (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Enumeration_Type;

   ------------------------------
   -- Is_Exception_Declaration --
   ------------------------------

   overriding function Is_Exception_Declaration (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Exception_Declaration;

   --------------------------
   -- Is_Exception_Handler --
   --------------------------

   overriding function Is_Exception_Handler (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Exception_Handler;

   ---------------------------------------
   -- Is_Exception_Renaming_Declaration --
   ---------------------------------------

   overriding function Is_Exception_Renaming_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Exception_Renaming_Declaration;

   -----------------------
   -- Is_Exit_Statement --
   -----------------------

   overriding function Is_Exit_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Exit_Statement;

   -----------------------------
   -- Is_Explicit_Dereference --
   -----------------------------

   overriding function Is_Explicit_Dereference (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Explicit_Dereference;

   -------------------
   -- Is_Expression --
   -------------------

   overriding function Is_Expression (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Expression;

   ----------------------------------
   -- Is_Extended_Return_Statement --
   ----------------------------------

   overriding function Is_Extended_Return_Statement
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Extended_Return_Statement;

   ----------------------------
   -- Is_Extension_Aggregate --
   ----------------------------

   overriding function Is_Extension_Aggregate (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Extension_Aggregate;

   ----------------------------
   -- Is_Floating_Point_Type --
   ----------------------------

   overriding function Is_Floating_Point_Type (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Floating_Point_Type;

   ---------------------------
   -- Is_For_Loop_Statement --
   ---------------------------

   overriding function Is_For_Loop_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_For_Loop_Statement;

   ---------------------------
   -- Is_Formal_Access_Type --
   ---------------------------

   overriding function Is_Formal_Access_Type (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Access_Type;

   --------------------------------------
   -- Is_Formal_Constrained_Array_Type --
   --------------------------------------

   overriding function Is_Formal_Constrained_Array_Type
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Constrained_Array_Type;

   ----------------------------------------------
   -- Is_Formal_Decimal_Fixed_Point_Definition --
   ----------------------------------------------

   overriding function Is_Formal_Decimal_Fixed_Point_Definition
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Decimal_Fixed_Point_Definition;

   ---------------------------------------
   -- Is_Formal_Derived_Type_Definition --
   ---------------------------------------

   overriding function Is_Formal_Derived_Type_Definition
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Derived_Type_Definition;

   ----------------------------------------
   -- Is_Formal_Discrete_Type_Definition --
   ----------------------------------------

   overriding function Is_Formal_Discrete_Type_Definition
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Discrete_Type_Definition;

   -----------------------------------------
   -- Is_Formal_Floating_Point_Definition --
   -----------------------------------------

   overriding function Is_Formal_Floating_Point_Definition
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Floating_Point_Definition;

   ------------------------------------
   -- Is_Formal_Function_Declaration --
   ------------------------------------

   overriding function Is_Formal_Function_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Function_Declaration;

   ---------------------------------------
   -- Is_Formal_Modular_Type_Definition --
   ---------------------------------------

   overriding function Is_Formal_Modular_Type_Definition
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Modular_Type_Definition;

   ----------------------------------
   -- Is_Formal_Object_Access_Type --
   ----------------------------------

   overriding function Is_Formal_Object_Access_Type
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Object_Access_Type;

   -------------------------------------
   -- Is_Formal_Procedure_Access_Type --
   -------------------------------------

   overriding function Is_Formal_Procedure_Access_Type
     (Self : Node)  return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Procedure_Access_Type;

   ------------------------------------
   -- Is_Formal_Function_Access_Type --
   ------------------------------------

   overriding function Is_Formal_Function_Access_Type
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Function_Access_Type;

   ------------------------------
   -- Is_Formal_Interface_Type --
   ------------------------------

   overriding function Is_Formal_Interface_Type (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Interface_Type;

   ----------------------------------
   -- Is_Formal_Object_Declaration --
   ----------------------------------

   overriding function Is_Formal_Object_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Object_Declaration;

   -----------------------------------------------
   -- Is_Formal_Ordinary_Fixed_Point_Definition --
   -----------------------------------------------

   overriding function Is_Formal_Ordinary_Fixed_Point_Definition
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Ordinary_Fixed_Point_Definition;

   -----------------------------------
   -- Is_Formal_Package_Association --
   -----------------------------------

   overriding function Is_Formal_Package_Association
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Package_Association;

   -----------------------------------
   -- Is_Formal_Package_Declaration --
   -----------------------------------

   overriding function Is_Formal_Package_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Package_Declaration;

   ---------------------------------------
   -- Is_Formal_Private_Type_Definition --
   ---------------------------------------

   overriding function Is_Formal_Private_Type_Definition
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Private_Type_Definition;

   -------------------------------------
   -- Is_Formal_Procedure_Declaration --
   -------------------------------------

   overriding function Is_Formal_Procedure_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Procedure_Declaration;

   ----------------------------------------------
   -- Is_Formal_Signed_Integer_Type_Definition --
   ----------------------------------------------

   overriding function Is_Formal_Signed_Integer_Type_Definition
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Signed_Integer_Type_Definition;

   --------------------------------
   -- Is_Formal_Type_Declaration --
   --------------------------------

   overriding function Is_Formal_Type_Declaration (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Type_Declaration;

   -------------------------------
   -- Is_Formal_Type_Definition --
   -------------------------------

   overriding function Is_Formal_Type_Definition (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Type_Definition;

   ----------------------------------------
   -- Is_Formal_Unconstrained_Array_Type --
   ----------------------------------------

   overriding function Is_Formal_Unconstrained_Array_Type
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Unconstrained_Array_Type;

   -----------------------------
   -- Is_Function_Access_Type --
   -----------------------------

   overriding function Is_Function_Access_Type (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Function_Access_Type;

   ----------------------------------
   -- Is_Function_Body_Declaration --
   ----------------------------------

   overriding function Is_Function_Body_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Function_Body_Declaration;

   ---------------------------
   -- Is_Function_Body_Stub --
   ---------------------------

   overriding function Is_Function_Body_Stub (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Function_Body_Stub;

   ----------------------
   -- Is_Function_Call --
   ----------------------

   overriding function Is_Function_Call (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Function_Call;

   -----------------------------
   -- Is_Function_Declaration --
   -----------------------------

   overriding function Is_Function_Declaration (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Function_Declaration;

   -------------------------------
   -- Is_Function_Instantiation --
   -------------------------------

   overriding function Is_Function_Instantiation (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Function_Instantiation;

   --------------------------------------
   -- Is_Function_Renaming_Declaration --
   --------------------------------------

   overriding function Is_Function_Renaming_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Function_Renaming_Declaration;

   -------------------------------------------
   -- Is_Generalized_Iterator_Specification --
   -------------------------------------------

   overriding function Is_Generalized_Iterator_Specification
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Generalized_Iterator_Specification;

   -------------------------------------
   -- Is_Generic_Function_Declaration --
   -------------------------------------

   overriding function Is_Generic_Function_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Generic_Function_Declaration;

   ----------------------------------------------
   -- Is_Generic_Function_Renaming_Declaration --
   ----------------------------------------------

   overriding function Is_Generic_Function_Renaming_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Generic_Function_Renaming_Declaration;

   ------------------------------------
   -- Is_Generic_Package_Declaration --
   ------------------------------------

   overriding function Is_Generic_Package_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Generic_Package_Declaration;

   ---------------------------------------------
   -- Is_Generic_Package_Renaming_Declaration --
   ---------------------------------------------

   overriding function Is_Generic_Package_Renaming_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Generic_Package_Renaming_Declaration;

   --------------------------------------
   -- Is_Generic_Procedure_Declaration --
   --------------------------------------

   overriding function Is_Generic_Procedure_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Generic_Procedure_Declaration;

   -----------------------------------------------
   -- Is_Generic_Procedure_Renaming_Declaration --
   -----------------------------------------------

   overriding function Is_Generic_Procedure_Renaming_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Generic_Procedure_Renaming_Declaration;

   -----------------------
   -- Is_Goto_Statement --
   -----------------------

   overriding function Is_Goto_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Goto_Statement;

   -------------------
   -- Is_Identifier --
   -------------------

   overriding function Is_Identifier (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Identifier;

   ----------------------
   -- Is_If_Expression --
   ----------------------

   overriding function Is_If_Expression (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_If_Expression;

   ---------------------
   -- Is_If_Statement --
   ---------------------

   overriding function Is_If_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_If_Statement;

   -----------------------------------
   -- Is_Incomplete_Type_Definition --
   -----------------------------------

   overriding function Is_Incomplete_Type_Definition
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Incomplete_Type_Definition;

   -------------------------
   -- Is_Index_Constraint --
   -------------------------

   overriding function Is_Index_Constraint (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Index_Constraint;

   --------------------------
   -- Is_Indexed_Component --
   --------------------------

   overriding function Is_Indexed_Component (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Indexed_Component;

   -----------------------
   -- Is_Infix_Operator --
   -----------------------

   overriding function Is_Infix_Operator (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Infix_Operator;

   -----------------------
   -- Is_Interface_Type --
   -----------------------

   overriding function Is_Interface_Type (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Interface_Type;

   --------------------------------
   -- Is_Known_Discriminant_Part --
   --------------------------------

   overriding function Is_Known_Discriminant_Part (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Known_Discriminant_Part;

   -------------------------------------
   -- Is_Loop_Parameter_Specification --
   -------------------------------------

   overriding function Is_Loop_Parameter_Specification
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Loop_Parameter_Specification;

   -----------------------
   -- Is_Loop_Statement --
   -----------------------

   overriding function Is_Loop_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Loop_Statement;

   ------------------------
   -- Is_Membership_Test --
   ------------------------

   overriding function Is_Membership_Test (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Membership_Test;

   ---------------------
   -- Is_Modular_Type --
   ---------------------

   overriding function Is_Modular_Type (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Modular_Type;

   -----------------------
   -- Is_Null_Component --
   -----------------------

   overriding function Is_Null_Component (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Null_Component;

   ---------------------
   -- Is_Null_Literal --
   ---------------------

   overriding function Is_Null_Literal (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Null_Literal;

   -----------------------
   -- Is_Null_Statement --
   -----------------------

   overriding function Is_Null_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Null_Statement;

   ---------------------------
   -- Is_Number_Declaration --
   ---------------------------

   overriding function Is_Number_Declaration (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Number_Declaration;

   ------------------------
   -- Is_Numeric_Literal --
   ------------------------

   overriding function Is_Numeric_Literal (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Numeric_Literal;

   ---------------------------
   -- Is_Object_Access_Type --
   ---------------------------

   overriding function Is_Object_Access_Type (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Object_Access_Type;

   ---------------------------
   -- Is_Object_Declaration --
   ---------------------------

   overriding function Is_Object_Declaration (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Object_Declaration;

   ------------------------------------
   -- Is_Object_Renaming_Declaration --
   ------------------------------------

   overriding function Is_Object_Renaming_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Object_Renaming_Declaration;

   ------------------------
   -- Is_Operator_Symbol --
   ------------------------

   overriding function Is_Operator_Symbol (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Operator_Symbol;

   ----------------------------------
   -- Is_Ordinary_Fixed_Point_Type --
   ----------------------------------

   overriding function Is_Ordinary_Fixed_Point_Type
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Ordinary_Fixed_Point_Type;

   ----------------------
   -- Is_Others_Choice --
   ----------------------

   overriding function Is_Others_Choice (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Others_Choice;

   ---------------------------------
   -- Is_Package_Body_Declaration --
   ---------------------------------

   overriding function Is_Package_Body_Declaration (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Package_Body_Declaration;

   --------------------------
   -- Is_Package_Body_Stub --
   --------------------------

   overriding function Is_Package_Body_Stub (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Package_Body_Stub;

   ----------------------------
   -- Is_Package_Declaration --
   ----------------------------

   overriding function Is_Package_Declaration (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Package_Declaration;

   ------------------------------
   -- Is_Package_Instantiation --
   ------------------------------

   overriding function Is_Package_Instantiation (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Package_Instantiation;

   -------------------------------------
   -- Is_Package_Renaming_Declaration --
   -------------------------------------

   overriding function Is_Package_Renaming_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Package_Renaming_Declaration;

   ------------------------------
   -- Is_Parameter_Association --
   ------------------------------

   overriding function Is_Parameter_Association (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Parameter_Association;

   --------------------------------
   -- Is_Parameter_Specification --
   --------------------------------

   overriding function Is_Parameter_Specification (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Parameter_Specification;

   ---------------------------------
   -- Is_Parenthesized_Expression --
   ---------------------------------

   overriding function Is_Parenthesized_Expression (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Parenthesized_Expression;

   -------------------------
   -- Is_Part_Of_Implicit --
   -------------------------

   overriding function Is_Part_Of_Implicit (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Part_Of_Implicit;

   --------------------------
   -- Is_Part_Of_Inherited --
   --------------------------

   overriding function Is_Part_Of_Inherited (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Part_Of_Inherited;

   -------------------------
   -- Is_Part_Of_Instance --
   -------------------------

   overriding function Is_Part_Of_Instance (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Part_Of_Instance;

   -------------
   -- Is_Path --
   -------------

   overriding function Is_Path (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Path;

   ---------------
   -- Is_Pragma --
   ---------------

   overriding function Is_Pragma (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Pragma;

   -------------------------------------
   -- Is_Private_Extension_Definition --
   -------------------------------------

   overriding function Is_Private_Extension_Definition
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Private_Extension_Definition;

   --------------------------------
   -- Is_Private_Type_Definition --
   --------------------------------

   overriding function Is_Private_Type_Definition (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Private_Type_Definition;

   ------------------------------
   -- Is_Procedure_Access_Type --
   ------------------------------

   overriding function Is_Procedure_Access_Type (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Procedure_Access_Type;

   -----------------------------------
   -- Is_Procedure_Body_Declaration --
   -----------------------------------

   overriding function Is_Procedure_Body_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Procedure_Body_Declaration;

   ----------------------------
   -- Is_Procedure_Body_Stub --
   ----------------------------

   overriding function Is_Procedure_Body_Stub (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Procedure_Body_Stub;

   ------------------------------
   -- Is_Procedure_Declaration --
   ------------------------------

   overriding function Is_Procedure_Declaration (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Procedure_Declaration;

   --------------------------------
   -- Is_Procedure_Instantiation --
   --------------------------------

   overriding function Is_Procedure_Instantiation (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Procedure_Instantiation;

   ---------------------------------------
   -- Is_Procedure_Renaming_Declaration --
   ---------------------------------------

   overriding function Is_Procedure_Renaming_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Procedure_Renaming_Declaration;

   -----------------------------------
   -- Is_Protected_Body_Declaration --
   -----------------------------------

   overriding function Is_Protected_Body_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Protected_Body_Declaration;

   ----------------------------
   -- Is_Protected_Body_Stub --
   ----------------------------

   overriding function Is_Protected_Body_Stub (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Protected_Body_Stub;

   -----------------------------
   -- Is_Protected_Definition --
   -----------------------------

   overriding function Is_Protected_Definition (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Protected_Definition;

   -----------------------------------
   -- Is_Protected_Type_Declaration --
   -----------------------------------

   overriding function Is_Protected_Type_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Protected_Type_Declaration;

   -----------------------------
   -- Is_Qualified_Expression --
   -----------------------------

   overriding function Is_Qualified_Expression (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Qualified_Expression;

   ------------------------------
   -- Is_Quantified_Expression --
   ------------------------------

   overriding function Is_Quantified_Expression (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Quantified_Expression;

   -------------------------
   -- Is_Raise_Expression --
   -------------------------

   overriding function Is_Raise_Expression (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Raise_Expression;

   ------------------------
   -- Is_Raise_Statement --
   ------------------------

   overriding function Is_Raise_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Raise_Statement;

   ----------------------------------
   -- Is_Range_Attribute_Reference --
   ----------------------------------

   overriding function Is_Range_Attribute_Reference
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Range_Attribute_Reference;

   ---------------------------------
   -- Is_Real_Range_Specification --
   ---------------------------------

   overriding function Is_Real_Range_Specification (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Real_Range_Specification;

   -------------------------
   -- Is_Record_Aggregate --
   -------------------------

   overriding function Is_Record_Aggregate (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Record_Aggregate;

   -------------------------------------
   -- Is_Record_Component_Association --
   -------------------------------------

   overriding function Is_Record_Component_Association
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Record_Component_Association;

   --------------------------
   -- Is_Record_Definition --
   --------------------------

   overriding function Is_Record_Definition (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Record_Definition;

   -------------------------------------
   -- Is_Record_Representation_Clause --
   -------------------------------------

   overriding function Is_Record_Representation_Clause
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Record_Representation_Clause;

   --------------------
   -- Is_Record_Type --
   --------------------

   overriding function Is_Record_Type (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Record_Type;

   ------------------------------
   -- Is_Representation_Clause --
   ------------------------------

   overriding function Is_Representation_Clause (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Representation_Clause;

   --------------------------
   -- Is_Requeue_Statement --
   --------------------------

   overriding function Is_Requeue_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Requeue_Statement;

   ------------------------------------
   -- Is_Return_Object_Specification --
   ------------------------------------

   overriding function Is_Return_Object_Specification
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Return_Object_Specification;

   ------------------
   -- Is_Root_Type --
   ------------------

   overriding function Is_Root_Type (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Root_Type;

   --------------------
   -- Is_Select_Path --
   --------------------

   overriding function Is_Select_Path (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Select_Path;

   -------------------------
   -- Is_Select_Statement --
   -------------------------

   overriding function Is_Select_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Select_Statement;

   ---------------------------
   -- Is_Selected_Component --
   ---------------------------

   overriding function Is_Selected_Component (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Selected_Component;

   --------------------------------
   -- Is_Short_Circuit_Operation --
   --------------------------------

   overriding function Is_Short_Circuit_Operation (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Short_Circuit_Operation;

   ----------------------------
   -- Is_Signed_Integer_Type --
   ----------------------------

   overriding function Is_Signed_Integer_Type (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Signed_Integer_Type;

   --------------------------------
   -- Is_Simple_Expression_Range --
   --------------------------------

   overriding function Is_Simple_Expression_Range (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Simple_Expression_Range;

   --------------------------------
   -- Is_Simple_Return_Statement --
   --------------------------------

   overriding function Is_Simple_Return_Statement (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Simple_Return_Statement;

   -------------------------------------
   -- Is_Single_Protected_Declaration --
   -------------------------------------

   overriding function Is_Single_Protected_Declaration
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Single_Protected_Declaration;

   --------------------------------
   -- Is_Single_Task_Declaration --
   --------------------------------

   overriding function Is_Single_Task_Declaration (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Single_Task_Declaration;

   --------------
   -- Is_Slice --
   --------------

   overriding function Is_Slice (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Slice;

   ------------------
   -- Is_Statement --
   ------------------

   overriding function Is_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Statement;

   -----------------------
   -- Is_String_Literal --
   -----------------------

   overriding function Is_String_Literal (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_String_Literal;

   ----------------------------
   -- Is_Subtype_Declaration --
   ----------------------------

   overriding function Is_Subtype_Declaration (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Subtype_Declaration;

   ---------------------------
   -- Is_Subtype_Indication --
   ---------------------------

   overriding function Is_Subtype_Indication (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Subtype_Indication;

   ------------------------------
   -- Is_Task_Body_Declaration --
   ------------------------------

   overriding function Is_Task_Body_Declaration (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Task_Body_Declaration;

   -----------------------
   -- Is_Task_Body_Stub --
   -----------------------

   overriding function Is_Task_Body_Stub (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Task_Body_Stub;

   ------------------------
   -- Is_Task_Definition --
   ------------------------

   overriding function Is_Task_Definition (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Task_Definition;

   ------------------------------
   -- Is_Task_Type_Declaration --
   ------------------------------

   overriding function Is_Task_Type_Declaration (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Task_Type_Declaration;

   ----------------------------------------
   -- Is_Terminate_Alternative_Statement --
   ----------------------------------------

   overriding function Is_Terminate_Alternative_Statement
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Terminate_Alternative_Statement;

   ------------------------
   -- Is_Type_Conversion --
   ------------------------

   overriding function Is_Type_Conversion (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Type_Conversion;

   -------------------------
   -- Is_Type_Declaration --
   -------------------------

   overriding function Is_Type_Declaration (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Type_Declaration;

   ------------------------
   -- Is_Type_Definition --
   ------------------------

   overriding function Is_Type_Definition (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Type_Definition;

   ---------------------------------
   -- Is_Unconstrained_Array_Type --
   ---------------------------------

   overriding function Is_Unconstrained_Array_Type (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Unconstrained_Array_Type;

   ----------------------------------
   -- Is_Unknown_Discriminant_Part --
   ----------------------------------

   overriding function Is_Unknown_Discriminant_Part
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Unknown_Discriminant_Part;

   -------------------
   -- Is_Use_Clause --
   -------------------

   overriding function Is_Use_Clause (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Use_Clause;

   ----------------
   -- Is_Variant --
   ----------------

   overriding function Is_Variant (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Variant;

   ---------------------
   -- Is_Variant_Part --
   ---------------------

   overriding function Is_Variant_Part (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Variant_Part;

   -----------------------------
   -- Is_While_Loop_Statement --
   -----------------------------

   overriding function Is_While_Loop_Statement (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_While_Loop_Statement;

   --------------------
   -- Is_With_Clause --
   --------------------

   overriding function Is_With_Clause (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_With_Clause;

   ---------------------------
   -- Set_Enclosing_Element --
   ---------------------------

   procedure Set_Enclosing_Element
     (Self  : access Program.Elements.Element'Class;
      Value : access Program.Elements.Element'Class) is
   begin
      if Self.all in Node'Class then
         Node'Class (Self.all).Enclosing_Element := Value;
      end if;
   end Set_Enclosing_Element;

end Program.Nodes;
