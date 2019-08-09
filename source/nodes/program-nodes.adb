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
     (Self : Node) return Program.Elements.Element_Access
   is
   begin
      return Self.Enclosing_Element;
   end Enclosing_Element;

   ------------------------
   -- Is_Abort_Statement --
   ------------------------

   overriding function Is_Abort_Statement_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Abort_Statement_Element;

   -------------------------
   -- Is_Accept_Statement --
   -------------------------

   overriding function Is_Accept_Statement_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Accept_Statement_Element;

   --------------------
   -- Is_Access_Type --
   --------------------

   overriding function Is_Access_Type_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Access_Type_Element;

   ------------------
   -- Is_Allocator --
   ------------------

   overriding function Is_Allocator_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Allocator_Element;

   ------------------------------------
   -- Is_Anonymous_Access_Definition --
   ------------------------------------

   overriding function Is_Anonymous_Access_Definition_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Anonymous_Access_Definition_Element;

   -----------------------------------
   -- Is_Anonymous_Access_To_Object --
   -----------------------------------

   overriding function Is_Anonymous_Access_To_Object_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Anonymous_Access_To_Object_Element;

   --------------------------------------
   -- Is_Anonymous_Access_To_Procedure --
   --------------------------------------

   overriding function Is_Anonymous_Access_To_Procedure_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Anonymous_Access_To_Procedure_Element;

   -------------------------------------
   -- Is_Anonymous_Access_To_Function --
   -------------------------------------

   overriding function Is_Anonymous_Access_To_Function_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Anonymous_Access_To_Function_Element;

   ------------------------
   -- Is_Array_Aggregate --
   ------------------------

   overriding function Is_Array_Aggregate_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Array_Aggregate_Element;

   ------------------------------------
   -- Is_Array_Component_Association --
   ------------------------------------

   overriding function Is_Array_Component_Association_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Array_Component_Association_Element;

   -----------------------------
   -- Is_Aspect_Specification --
   -----------------------------

   overriding function Is_Aspect_Specification_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Aspect_Specification_Element;

   -----------------------------
   -- Is_Assignment_Statement --
   -----------------------------

   overriding function Is_Assignment_Statement_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Assignment_Statement_Element;

   --------------------
   -- Is_Association --
   --------------------

   overriding function Is_Association_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Association_Element;

   ------------------
   -- Is_At_Clause --
   ------------------

   overriding function Is_At_Clause_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_At_Clause_Element;

   ------------------------------------
   -- Is_Attribute_Definition_Clause --
   ------------------------------------

   overriding function Is_Attribute_Definition_Clause_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Attribute_Definition_Clause_Element;

   ----------------------------
   -- Is_Attribute_Reference --
   ----------------------------

   overriding function Is_Attribute_Reference_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Attribute_Reference_Element;

   ------------------------
   -- Is_Block_Statement --
   ------------------------

   overriding function Is_Block_Statement_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Block_Statement_Element;

   -----------------------
   -- Is_Call_Statement --
   -----------------------

   overriding function Is_Call_Statement_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Call_Statement_Element;

   ------------------------
   -- Is_Case_Expression --
   ------------------------

   overriding function Is_Case_Expression_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Case_Expression_Element;

   -----------------------------
   -- Is_Case_Expression_Path --
   -----------------------------

   overriding function Is_Case_Expression_Path_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Case_Expression_Path_Element;

   ------------------
   -- Is_Case_Path --
   ------------------

   overriding function Is_Case_Path_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Case_Path_Element;

   -----------------------
   -- Is_Case_Statement --
   -----------------------

   overriding function Is_Case_Statement_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Case_Statement_Element;

   --------------------------
   -- Is_Character_Literal --
   --------------------------

   overriding function Is_Character_Literal_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Character_Literal_Element;

   ---------------------------------------
   -- Is_Choice_Parameter_Specification --
   ---------------------------------------

   overriding function Is_Choice_Parameter_Specification_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Choice_Parameter_Specification_Element;

   ---------------
   -- Is_Clause --
   ---------------

   overriding function Is_Clause_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Clause_Element;

   -----------------------
   -- Is_Code_Statement --
   -----------------------

   overriding function Is_Code_Statement_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Code_Statement_Element;

   -------------------------
   -- Is_Component_Clause --
   -------------------------

   overriding function Is_Component_Clause_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Component_Clause_Element;

   ------------------------------
   -- Is_Component_Declaration --
   ------------------------------

   overriding function Is_Component_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Component_Declaration_Element;

   -----------------------------
   -- Is_Component_Definition --
   -----------------------------

   overriding function Is_Component_Definition_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Component_Definition_Element;

   -------------------------------
   -- Is_Constrained_Array_Type --
   -------------------------------

   overriding function Is_Constrained_Array_Type_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Constrained_Array_Type_Element;

   -------------------
   -- Is_Constraint --
   -------------------

   overriding function Is_Constraint_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Constraint_Element;

   ---------------------------------
   -- Is_Decimal_Fixed_Point_Type --
   ---------------------------------

   overriding function Is_Decimal_Fixed_Point_Type_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Decimal_Fixed_Point_Type_Element;

   --------------------
   -- Is_Declaration --
   --------------------

   overriding function Is_Declaration_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Declaration_Element;

   -----------------------------------
   -- Is_Defining_Character_Literal --
   -----------------------------------

   overriding function Is_Defining_Character_Literal_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Defining_Character_Literal_Element;

   -------------------------------
   -- Is_Defining_Expanded_Name --
   -------------------------------

   overriding function Is_Defining_Expanded_Name_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Defining_Expanded_Name_Element;

   ----------------------------
   -- Is_Defining_Identifier --
   ----------------------------

   overriding function Is_Defining_Identifier_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Defining_Identifier_Element;

   ----------------------
   -- Is_Defining_Name --
   ----------------------

   overriding function Is_Defining_Name_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Defining_Name_Element;

   ---------------------------------
   -- Is_Defining_Operator_Symbol --
   ---------------------------------

   overriding function Is_Defining_Operator_Symbol_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Defining_Operator_Symbol_Element;

   -------------------
   -- Is_Definition --
   -------------------

   overriding function Is_Definition_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Definition_Element;

   ------------------------
   -- Is_Delay_Statement --
   ------------------------

   overriding function Is_Delay_Statement_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Delay_Statement_Element;

   -------------------------
   -- Is_Delta_Constraint --
   -------------------------

   overriding function Is_Delta_Constraint_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Delta_Constraint_Element;

   ---------------------------------
   -- Is_Derived_Record_Extension --
   ---------------------------------

   overriding function Is_Derived_Record_Extension_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Derived_Record_Extension_Element;

   ---------------------
   -- Is_Derived_Type --
   ---------------------

   overriding function Is_Derived_Type_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Derived_Type_Element;

   --------------------------
   -- Is_Digits_Constraint --
   --------------------------

   overriding function Is_Digits_Constraint_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Digits_Constraint_Element;

   -----------------------
   -- Is_Discrete_Range --
   -----------------------

   overriding function Is_Discrete_Range_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Discrete_Range_Element;

   -------------------------------------------
   -- Is_Discrete_Range_Attribute_Reference --
   -------------------------------------------

   overriding function Is_Discrete_Range_Attribute_Reference_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Discrete_Range_Attribute_Reference_Element;

   -----------------------------------------
   -- Is_Discrete_Simple_Expression_Range --
   -----------------------------------------

   overriding function Is_Discrete_Simple_Expression_Range_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Discrete_Simple_Expression_Range_Element;

   ------------------------------------
   -- Is_Discrete_Subtype_Indication --
   ------------------------------------

   overriding function Is_Discrete_Subtype_Indication_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Discrete_Subtype_Indication_Element;

   ---------------------------------
   -- Is_Discriminant_Association --
   ---------------------------------

   overriding function Is_Discriminant_Association_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Discriminant_Association_Element;

   --------------------------------
   -- Is_Discriminant_Constraint --
   --------------------------------

   overriding function Is_Discriminant_Constraint_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Discriminant_Constraint_Element;

   -----------------------------------
   -- Is_Discriminant_Specification --
   -----------------------------------

   overriding function Is_Discriminant_Specification_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Discriminant_Specification_Element;

   ---------------------------------------
   -- Is_Element_Iterator_Specification --
   ---------------------------------------

   overriding function Is_Element_Iterator_Specification_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Element_Iterator_Specification_Element;

   ------------------------------
   -- Is_Elsif_Expression_Path --
   ------------------------------

   overriding function Is_Elsif_Expression_Path_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Elsif_Expression_Path_Element;

   -------------------
   -- Is_Elsif_Path --
   -------------------

   overriding function Is_Elsif_Path_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Elsif_Path_Element;

   -------------------------------
   -- Is_Entry_Body_Declaration --
   -------------------------------

   overriding function Is_Entry_Body_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Entry_Body_Declaration_Element;

   --------------------------
   -- Is_Entry_Declaration --
   --------------------------

   overriding function Is_Entry_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Entry_Declaration_Element;

   ----------------------------------
   -- Is_Entry_Index_Specification --
   ----------------------------------

   overriding function Is_Entry_Index_Specification_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Entry_Index_Specification_Element;

   ------------------------------------------
   -- Is_Enumeration_Literal_Specification --
   ------------------------------------------

   overriding function Is_Enumeration_Literal_Specification_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Enumeration_Literal_Specification_Element;

   ------------------------------------------
   -- Is_Enumeration_Representation_Clause --
   ------------------------------------------

   overriding function Is_Enumeration_Representation_Clause_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Enumeration_Representation_Clause_Element;

   -------------------------
   -- Is_Enumeration_Type --
   -------------------------

   overriding function Is_Enumeration_Type_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Enumeration_Type_Element;

   ------------------------------
   -- Is_Exception_Declaration --
   ------------------------------

   overriding function Is_Exception_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Exception_Declaration_Element;

   --------------------------
   -- Is_Exception_Handler --
   --------------------------

   overriding function Is_Exception_Handler_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Exception_Handler_Element;

   ---------------------------------------
   -- Is_Exception_Renaming_Declaration --
   ---------------------------------------

   overriding function Is_Exception_Renaming_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Exception_Renaming_Declaration_Element;

   -----------------------
   -- Is_Exit_Statement --
   -----------------------

   overriding function Is_Exit_Statement_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Exit_Statement_Element;

   -----------------------------
   -- Is_Explicit_Dereference --
   -----------------------------

   overriding function Is_Explicit_Dereference_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Explicit_Dereference_Element;

   -------------------
   -- Is_Expression --
   -------------------

   overriding function Is_Expression_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Expression_Element;

   ----------------------------------
   -- Is_Extended_Return_Statement --
   ----------------------------------

   overriding function Is_Extended_Return_Statement_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Extended_Return_Statement_Element;

   ----------------------------
   -- Is_Extension_Aggregate --
   ----------------------------

   overriding function Is_Extension_Aggregate_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Extension_Aggregate_Element;

   ----------------------------
   -- Is_Floating_Point_Type --
   ----------------------------

   overriding function Is_Floating_Point_Type_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Floating_Point_Type_Element;

   ---------------------------
   -- Is_For_Loop_Statement --
   ---------------------------

   overriding function Is_For_Loop_Statement_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_For_Loop_Statement_Element;

   ---------------------------
   -- Is_Formal_Access_Type --
   ---------------------------

   overriding function Is_Formal_Access_Type_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Access_Type_Element;

   --------------------------------------
   -- Is_Formal_Constrained_Array_Type --
   --------------------------------------

   overriding function Is_Formal_Constrained_Array_Type_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Constrained_Array_Type_Element;

   ----------------------------------------------
   -- Is_Formal_Decimal_Fixed_Point_Definition --
   ----------------------------------------------

   overriding function Is_Formal_Decimal_Fixed_Point_Definition_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Decimal_Fixed_Point_Definition_Element;

   ---------------------------------------
   -- Is_Formal_Derived_Type_Definition --
   ---------------------------------------

   overriding function Is_Formal_Derived_Type_Definition_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Derived_Type_Definition_Element;

   ----------------------------------------
   -- Is_Formal_Discrete_Type_Definition --
   ----------------------------------------

   overriding function Is_Formal_Discrete_Type_Definition_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Discrete_Type_Definition_Element;

   -----------------------------------------
   -- Is_Formal_Floating_Point_Definition --
   -----------------------------------------

   overriding function Is_Formal_Floating_Point_Definition_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Floating_Point_Definition_Element;

   ------------------------------------
   -- Is_Formal_Function_Declaration --
   ------------------------------------

   overriding function Is_Formal_Function_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Function_Declaration_Element;

   ---------------------------------------
   -- Is_Formal_Modular_Type_Definition --
   ---------------------------------------

   overriding function Is_Formal_Modular_Type_Definition_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Modular_Type_Definition_Element;

   ----------------------------------
   -- Is_Formal_Object_Access_Type --
   ----------------------------------

   overriding function Is_Formal_Object_Access_Type_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Object_Access_Type_Element;

   -------------------------------------
   -- Is_Formal_Procedure_Access_Type --
   -------------------------------------

   overriding function Is_Formal_Procedure_Access_Type_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Procedure_Access_Type_Element;

   ------------------------------------
   -- Is_Formal_Function_Access_Type --
   ------------------------------------

   overriding function Is_Formal_Function_Access_Type_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Function_Access_Type_Element;

   ------------------------------
   -- Is_Formal_Interface_Type --
   ------------------------------

   overriding function Is_Formal_Interface_Type_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Interface_Type_Element;

   ----------------------------------
   -- Is_Formal_Object_Declaration --
   ----------------------------------

   overriding function Is_Formal_Object_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Object_Declaration_Element;

   -----------------------------------------------
   -- Is_Formal_Ordinary_Fixed_Point_Definition --
   -----------------------------------------------

   overriding function Is_Formal_Ordinary_Fixed_Point_Definition_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Ordinary_Fixed_Point_Definition_Element;

   -----------------------------------
   -- Is_Formal_Package_Association --
   -----------------------------------

   overriding function Is_Formal_Package_Association_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Package_Association_Element;

   -----------------------------------
   -- Is_Formal_Package_Declaration --
   -----------------------------------

   overriding function Is_Formal_Package_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Package_Declaration_Element;

   ---------------------------------------
   -- Is_Formal_Private_Type_Definition --
   ---------------------------------------

   overriding function Is_Formal_Private_Type_Definition_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Private_Type_Definition_Element;

   -------------------------------------
   -- Is_Formal_Procedure_Declaration --
   -------------------------------------

   overriding function Is_Formal_Procedure_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Procedure_Declaration_Element;

   ----------------------------------------------
   -- Is_Formal_Signed_Integer_Type_Definition --
   ----------------------------------------------

   overriding function Is_Formal_Signed_Integer_Type_Definition_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Signed_Integer_Type_Definition_Element;

   --------------------------------
   -- Is_Formal_Type_Declaration --
   --------------------------------

   overriding function Is_Formal_Type_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Type_Declaration_Element;

   -------------------------------
   -- Is_Formal_Type_Definition --
   -------------------------------

   overriding function Is_Formal_Type_Definition_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Type_Definition_Element;

   ----------------------------------------
   -- Is_Formal_Unconstrained_Array_Type --
   ----------------------------------------

   overriding function Is_Formal_Unconstrained_Array_Type_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Formal_Unconstrained_Array_Type_Element;

   -----------------------------
   -- Is_Function_Access_Type --
   -----------------------------

   overriding function Is_Function_Access_Type_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Function_Access_Type_Element;

   ----------------------------------
   -- Is_Function_Body_Declaration --
   ----------------------------------

   overriding function Is_Function_Body_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Function_Body_Declaration_Element;

   ---------------------------
   -- Is_Function_Body_Stub --
   ---------------------------

   overriding function Is_Function_Body_Stub_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Function_Body_Stub_Element;

   ----------------------
   -- Is_Function_Call --
   ----------------------

   overriding function Is_Function_Call_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Function_Call_Element;

   -----------------------------
   -- Is_Function_Declaration --
   -----------------------------

   overriding function Is_Function_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Function_Declaration_Element;

   -------------------------------
   -- Is_Function_Instantiation --
   -------------------------------

   overriding function Is_Function_Instantiation_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Function_Instantiation_Element;

   --------------------------------------
   -- Is_Function_Renaming_Declaration --
   --------------------------------------

   overriding function Is_Function_Renaming_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Function_Renaming_Declaration_Element;

   -------------------------------------------
   -- Is_Generalized_Iterator_Specification --
   -------------------------------------------

   overriding function Is_Generalized_Iterator_Specification_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Generalized_Iterator_Specification_Element;

   -------------------------------------
   -- Is_Generic_Function_Declaration --
   -------------------------------------

   overriding function Is_Generic_Function_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Generic_Function_Declaration_Element;

   ----------------------------------------------
   -- Is_Generic_Function_Renaming_Declaration --
   ----------------------------------------------

   overriding function Is_Generic_Function_Renaming_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Generic_Function_Renaming_Declaration_Element;

   ------------------------------------
   -- Is_Generic_Package_Declaration --
   ------------------------------------

   overriding function Is_Generic_Package_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Generic_Package_Declaration_Element;

   ---------------------------------------------
   -- Is_Generic_Package_Renaming_Declaration --
   ---------------------------------------------

   overriding function Is_Generic_Package_Renaming_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Generic_Package_Renaming_Declaration_Element;

   --------------------------------------
   -- Is_Generic_Procedure_Declaration --
   --------------------------------------

   overriding function Is_Generic_Procedure_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Generic_Procedure_Declaration_Element;

   -----------------------------------------------
   -- Is_Generic_Procedure_Renaming_Declaration --
   -----------------------------------------------

   overriding function Is_Generic_Procedure_Renaming_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Generic_Procedure_Renaming_Declaration_Element;

   -----------------------
   -- Is_Goto_Statement --
   -----------------------

   overriding function Is_Goto_Statement_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Goto_Statement_Element;

   -------------------
   -- Is_Identifier --
   -------------------

   overriding function Is_Identifier_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Identifier_Element;

   ----------------------
   -- Is_If_Expression --
   ----------------------

   overriding function Is_If_Expression_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_If_Expression_Element;

   ---------------------
   -- Is_If_Statement --
   ---------------------

   overriding function Is_If_Statement_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_If_Statement_Element;

   -----------------------------------
   -- Is_Incomplete_Type_Definition --
   -----------------------------------

   overriding function Is_Incomplete_Type_Definition_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Incomplete_Type_Definition_Element;

   -------------------------
   -- Is_Index_Constraint --
   -------------------------

   overriding function Is_Index_Constraint_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Index_Constraint_Element;

   --------------------------
   -- Is_Indexed_Component --
   --------------------------

   overriding function Is_Indexed_Component_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Indexed_Component_Element;

   -----------------------
   -- Is_Infix_Operator --
   -----------------------

   overriding function Is_Infix_Operator_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Infix_Operator_Element;

   -----------------------
   -- Is_Interface_Type --
   -----------------------

   overriding function Is_Interface_Type_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Interface_Type_Element;

   --------------------------------
   -- Is_Known_Discriminant_Part --
   --------------------------------

   overriding function Is_Known_Discriminant_Part_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Known_Discriminant_Part_Element;

   -------------------------------------
   -- Is_Loop_Parameter_Specification --
   -------------------------------------

   overriding function Is_Loop_Parameter_Specification_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Loop_Parameter_Specification_Element;

   -----------------------
   -- Is_Loop_Statement --
   -----------------------

   overriding function Is_Loop_Statement_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Loop_Statement_Element;

   ------------------------
   -- Is_Membership_Test --
   ------------------------

   overriding function Is_Membership_Test_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Membership_Test_Element;

   ---------------------
   -- Is_Modular_Type --
   ---------------------

   overriding function Is_Modular_Type_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Modular_Type_Element;

   -----------------------
   -- Is_Null_Component --
   -----------------------

   overriding function Is_Null_Component_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Null_Component_Element;

   ---------------------
   -- Is_Null_Literal --
   ---------------------

   overriding function Is_Null_Literal_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Null_Literal_Element;

   -----------------------
   -- Is_Null_Statement --
   -----------------------

   overriding function Is_Null_Statement_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Null_Statement_Element;

   ---------------------------
   -- Is_Number_Declaration --
   ---------------------------

   overriding function Is_Number_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Number_Declaration_Element;

   ------------------------
   -- Is_Numeric_Literal --
   ------------------------

   overriding function Is_Numeric_Literal_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Numeric_Literal_Element;

   ---------------------------
   -- Is_Object_Access_Type --
   ---------------------------

   overriding function Is_Object_Access_Type_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Object_Access_Type_Element;

   ---------------------------
   -- Is_Object_Declaration --
   ---------------------------

   overriding function Is_Object_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Object_Declaration_Element;

   ------------------------------------
   -- Is_Object_Renaming_Declaration --
   ------------------------------------

   overriding function Is_Object_Renaming_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Object_Renaming_Declaration_Element;

   ------------------------
   -- Is_Operator_Symbol --
   ------------------------

   overriding function Is_Operator_Symbol_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Operator_Symbol_Element;

   ----------------------------------
   -- Is_Ordinary_Fixed_Point_Type --
   ----------------------------------

   overriding function Is_Ordinary_Fixed_Point_Type_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Ordinary_Fixed_Point_Type_Element;

   ----------------------
   -- Is_Others_Choice --
   ----------------------

   overriding function Is_Others_Choice_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Others_Choice_Element;

   ---------------------------------
   -- Is_Package_Body_Declaration --
   ---------------------------------

   overriding function Is_Package_Body_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Package_Body_Declaration_Element;

   --------------------------
   -- Is_Package_Body_Stub --
   --------------------------

   overriding function Is_Package_Body_Stub_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Package_Body_Stub_Element;

   ----------------------------
   -- Is_Package_Declaration --
   ----------------------------

   overriding function Is_Package_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Package_Declaration_Element;

   ------------------------------
   -- Is_Package_Instantiation --
   ------------------------------

   overriding function Is_Package_Instantiation_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Package_Instantiation_Element;

   -------------------------------------
   -- Is_Package_Renaming_Declaration --
   -------------------------------------

   overriding function Is_Package_Renaming_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Package_Renaming_Declaration_Element;

   ------------------------------
   -- Is_Parameter_Association --
   ------------------------------

   overriding function Is_Parameter_Association_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Parameter_Association_Element;

   --------------------------------
   -- Is_Parameter_Specification --
   --------------------------------

   overriding function Is_Parameter_Specification_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Parameter_Specification_Element;

   ---------------------------------
   -- Is_Parenthesized_Expression --
   ---------------------------------

   overriding function Is_Parenthesized_Expression_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Parenthesized_Expression_Element;

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

   overriding function Is_Path_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Path_Element;

   ---------------
   -- Is_Pragma --
   ---------------

   overriding function Is_Pragma_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Pragma_Element;

   -------------------------------------
   -- Is_Private_Extension_Definition --
   -------------------------------------

   overriding function Is_Private_Extension_Definition_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Private_Extension_Definition_Element;

   --------------------------------
   -- Is_Private_Type_Definition --
   --------------------------------

   overriding function Is_Private_Type_Definition_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Private_Type_Definition_Element;

   ------------------------------
   -- Is_Procedure_Access_Type --
   ------------------------------

   overriding function Is_Procedure_Access_Type_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Procedure_Access_Type_Element;

   -----------------------------------
   -- Is_Procedure_Body_Declaration --
   -----------------------------------

   overriding function Is_Procedure_Body_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Procedure_Body_Declaration_Element;

   ----------------------------
   -- Is_Procedure_Body_Stub --
   ----------------------------

   overriding function Is_Procedure_Body_Stub_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Procedure_Body_Stub_Element;

   ------------------------------
   -- Is_Procedure_Declaration --
   ------------------------------

   overriding function Is_Procedure_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Procedure_Declaration_Element;

   --------------------------------
   -- Is_Procedure_Instantiation --
   --------------------------------

   overriding function Is_Procedure_Instantiation_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Procedure_Instantiation_Element;

   ---------------------------------------
   -- Is_Procedure_Renaming_Declaration --
   ---------------------------------------

   overriding function Is_Procedure_Renaming_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Procedure_Renaming_Declaration_Element;

   -----------------------------------
   -- Is_Protected_Body_Declaration --
   -----------------------------------

   overriding function Is_Protected_Body_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Protected_Body_Declaration_Element;

   ----------------------------
   -- Is_Protected_Body_Stub --
   ----------------------------

   overriding function Is_Protected_Body_Stub_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Protected_Body_Stub_Element;

   -----------------------------
   -- Is_Protected_Definition --
   -----------------------------

   overriding function Is_Protected_Definition_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Protected_Definition_Element;

   -----------------------------------
   -- Is_Protected_Type_Declaration --
   -----------------------------------

   overriding function Is_Protected_Type_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Protected_Type_Declaration_Element;

   -----------------------------
   -- Is_Qualified_Expression --
   -----------------------------

   overriding function Is_Qualified_Expression_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Qualified_Expression_Element;

   ------------------------------
   -- Is_Quantified_Expression --
   ------------------------------

   overriding function Is_Quantified_Expression_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Quantified_Expression_Element;

   -------------------------
   -- Is_Raise_Expression --
   -------------------------

   overriding function Is_Raise_Expression_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Raise_Expression_Element;

   ------------------------
   -- Is_Raise_Statement --
   ------------------------

   overriding function Is_Raise_Statement_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Raise_Statement_Element;

   ----------------------------------
   -- Is_Range_Attribute_Reference --
   ----------------------------------

   overriding function Is_Range_Attribute_Reference_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Range_Attribute_Reference_Element;

   ---------------------------------
   -- Is_Real_Range_Specification --
   ---------------------------------

   overriding function Is_Real_Range_Specification_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Real_Range_Specification_Element;

   -------------------------
   -- Is_Record_Aggregate --
   -------------------------

   overriding function Is_Record_Aggregate_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Record_Aggregate_Element;

   -------------------------------------
   -- Is_Record_Component_Association --
   -------------------------------------

   overriding function Is_Record_Component_Association_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Record_Component_Association_Element;

   --------------------------
   -- Is_Record_Definition --
   --------------------------

   overriding function Is_Record_Definition_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Record_Definition_Element;

   -------------------------------------
   -- Is_Record_Representation_Clause --
   -------------------------------------

   overriding function Is_Record_Representation_Clause_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Record_Representation_Clause_Element;

   --------------------
   -- Is_Record_Type --
   --------------------

   overriding function Is_Record_Type_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Record_Type_Element;

   ------------------------------
   -- Is_Representation_Clause --
   ------------------------------

   overriding function Is_Representation_Clause_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Representation_Clause_Element;

   --------------------------
   -- Is_Requeue_Statement --
   --------------------------

   overriding function Is_Requeue_Statement_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Requeue_Statement_Element;

   ------------------------------------
   -- Is_Return_Object_Specification --
   ------------------------------------

   overriding function Is_Return_Object_Specification_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Return_Object_Specification_Element;

   ------------------
   -- Is_Root_Type --
   ------------------

   overriding function Is_Root_Type_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Root_Type_Element;

   --------------------
   -- Is_Select_Path --
   --------------------

   overriding function Is_Select_Path_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Select_Path_Element;

   -------------------------
   -- Is_Select_Statement --
   -------------------------

   overriding function Is_Select_Statement_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Select_Statement_Element;

   ---------------------------
   -- Is_Selected_Component --
   ---------------------------

   overriding function Is_Selected_Component_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Selected_Component_Element;

   --------------------------------
   -- Is_Short_Circuit_Operation --
   --------------------------------

   overriding function Is_Short_Circuit_Operation_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Short_Circuit_Operation_Element;

   ----------------------------
   -- Is_Signed_Integer_Type --
   ----------------------------

   overriding function Is_Signed_Integer_Type_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Signed_Integer_Type_Element;

   --------------------------------
   -- Is_Simple_Expression_Range --
   --------------------------------

   overriding function Is_Simple_Expression_Range_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Simple_Expression_Range_Element;

   --------------------------------
   -- Is_Simple_Return_Statement --
   --------------------------------

   overriding function Is_Simple_Return_Statement_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Simple_Return_Statement_Element;

   -------------------------------------
   -- Is_Single_Protected_Declaration --
   -------------------------------------

   overriding function Is_Single_Protected_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Single_Protected_Declaration_Element;

   --------------------------------
   -- Is_Single_Task_Declaration --
   --------------------------------

   overriding function Is_Single_Task_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Single_Task_Declaration_Element;

   --------------
   -- Is_Slice --
   --------------

   overriding function Is_Slice_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Slice_Element;

   ------------------
   -- Is_Statement --
   ------------------

   overriding function Is_Statement_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Statement_Element;

   -----------------------
   -- Is_String_Literal --
   -----------------------

   overriding function Is_String_Literal_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_String_Literal_Element;

   ----------------------------
   -- Is_Subtype_Declaration --
   ----------------------------

   overriding function Is_Subtype_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Subtype_Declaration_Element;

   ---------------------------
   -- Is_Subtype_Indication --
   ---------------------------

   overriding function Is_Subtype_Indication_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Subtype_Indication_Element;

   ------------------------------
   -- Is_Task_Body_Declaration --
   ------------------------------

   overriding function Is_Task_Body_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Task_Body_Declaration_Element;

   -----------------------
   -- Is_Task_Body_Stub --
   -----------------------

   overriding function Is_Task_Body_Stub_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Task_Body_Stub_Element;

   ------------------------
   -- Is_Task_Definition --
   ------------------------

   overriding function Is_Task_Definition_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Task_Definition_Element;

   ------------------------------
   -- Is_Task_Type_Declaration --
   ------------------------------

   overriding function Is_Task_Type_Declaration_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Task_Type_Declaration_Element;

   ----------------------------------------
   -- Is_Terminate_Alternative_Statement --
   ----------------------------------------

   overriding function Is_Terminate_Alternative_Statement_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Terminate_Alternative_Statement_Element;

   ------------------------
   -- Is_Type_Conversion --
   ------------------------

   overriding function Is_Type_Conversion_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Type_Conversion_Element;

   -------------------------
   -- Is_Type_Declaration --
   -------------------------

   overriding function Is_Type_Declaration_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Type_Declaration_Element;

   ------------------------
   -- Is_Type_Definition --
   ------------------------

   overriding function Is_Type_Definition_Element (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Type_Definition_Element;

   ---------------------------------
   -- Is_Unconstrained_Array_Type --
   ---------------------------------

   overriding function Is_Unconstrained_Array_Type_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Unconstrained_Array_Type_Element;

   ----------------------------------
   -- Is_Unknown_Discriminant_Part --
   ----------------------------------

   overriding function Is_Unknown_Discriminant_Part_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Unknown_Discriminant_Part_Element;

   -------------------
   -- Is_Use_Clause --
   -------------------

   overriding function Is_Use_Clause_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Use_Clause_Element;

   ----------------
   -- Is_Variant --
   ----------------

   overriding function Is_Variant_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Variant_Element;

   ---------------------
   -- Is_Variant_Part --
   ---------------------

   overriding function Is_Variant_Part_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_Variant_Part_Element;

   -----------------------------
   -- Is_While_Loop_Statement --
   -----------------------------

   overriding function Is_While_Loop_Statement_Element
     (Self : Node) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_While_Loop_Statement_Element;

   --------------------
   -- Is_With_Clause --
   --------------------

   overriding function Is_With_Clause_Element (Self : Node) return Boolean is
      pragma Unreferenced (Self);
   begin
      return False;
   end Is_With_Clause_Element;

   ---------------------------
   -- Set_Enclosing_Element --
   ---------------------------

   procedure Set_Enclosing_Element
     (Self  : access Program.Elements.Element'Class;
      Value : access Program.Elements.Element'Class)
   is
   begin
      if Self.all in Node'Class then
         Node'Class (Self.all).Enclosing_Element := Value;
      end if;
   end Set_Enclosing_Element;

end Program.Nodes;
