------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                          http://gela.ada-ru.org                          --
--                     - - - - - - - - - - - - - - -                        --
--            Read copyright and license at the end of this file            --
------------------------------------------------------------------------------
--  $Revision$ $Date$
--  Purpose:
--  Procedural wrapper over Object-Oriented ASIS implementation

with Ada.Containers;

with Gela.Compilations;
with Gela.Compilation_Unit_Sets;
with Gela.Elements.Compilation_Units;
with Gela.Element_Visiters;
with Gela.Elements.Compilation_Unit_Bodies;
with Gela.Elements.Compilation_Unit_Declarations;
with Gela.Elements.Subunits;
with Gela.Elements.Context_Items;
with Asis.Extensions.Flat_Kinds;
with Gela.Elements.Library_Unit_Bodies;
with Gela.Elements.Library_Unit_Declarations;
with Gela.Elements.Proper_Bodies;
with Gela.Lexical_Types;

package body Asis.Elements is

   package F renames Asis.Extensions.Flat_Kinds;

   ----------------------------
   -- Access_Definition_Kind --
   ----------------------------

   function Access_Definition_Kind
     (Definition : Asis.Definition)
      return Asis.Access_Definition_Kinds
   is
      Map : constant array (F.An_Access_Definition)
        of Asis.Access_Definition_Kinds :=
          (F.An_Anonymous_Access_To_Variable =>
             Asis.An_Anonymous_Access_To_Variable,
           F.An_Anonymous_Access_To_Constant =>
             Asis.An_Anonymous_Access_To_Constant,
           F.An_Anonymous_Access_To_Procedure =>
             Asis.An_Anonymous_Access_To_Procedure,
           F.An_Anonymous_Access_To_Protected_Procedure =>
             Asis.An_Anonymous_Access_To_Protected_Procedure,
           F.An_Anonymous_Access_To_Function =>
             Asis.An_Anonymous_Access_To_Function,
           F.An_Anonymous_Access_To_Protected_Function =>
             Asis.An_Anonymous_Access_To_Protected_Function);

      Kind : constant Asis.Extensions.Flat_Kinds.Element_Flat_Kind :=
        Asis.Extensions.Flat_Kinds.Flat_Kind (Definition);
   begin
      if Kind in Map'Range then
         return Map (Kind);
      else
         return Not_An_Access_Definition;
      end if;
   end Access_Definition_Kind;

   ----------------------
   -- Access_Type_Kind --
   ----------------------

   function Access_Type_Kind
     (Definition : in Asis.Access_Type_Definition)
      return Asis.Access_Type_Kinds
   is
      Map : constant array (F.An_Access_Type_Definition)
        of Asis.Access_Type_Kinds :=
          (F.A_Pool_Specific_Access_To_Variable =>
             Asis.A_Pool_Specific_Access_To_Variable,
           F.An_Access_To_Variable =>
             Asis.An_Access_To_Variable,
           F.An_Access_To_Constant =>
             Asis.An_Access_To_Constant,
           F.An_Access_To_Procedure =>
             Asis.An_Access_To_Procedure,
           F.An_Access_To_Protected_Procedure =>
             Asis.An_Access_To_Protected_Procedure,
           F.An_Access_To_Function =>
             Asis.An_Access_To_Function,
           F.An_Access_To_Protected_Function =>
             Asis.An_Access_To_Protected_Function);

      Kind : constant Asis.Extensions.Flat_Kinds.Element_Flat_Kind :=
        Asis.Extensions.Flat_Kinds.Flat_Kind (Definition);
   begin
      if Kind in Map'Range then
         return Map (Kind);
      else
         return Not_An_Access_Type_Definition;
      end if;
   end Access_Type_Kind;

   ----------------------
   -- Association_Kind --
   ----------------------

   function Association_Kind
     (Association : in Asis.Association)
      return Asis.Association_Kinds
   is
      Map : constant array (F.An_Association) of Asis.Association_Kinds :=
        (F.A_Pragma_Argument_Association => Asis.A_Pragma_Argument_Association,
         F.A_Discriminant_Association => Asis.A_Discriminant_Association,
         F.A_Record_Component_Association =>
           Asis.A_Record_Component_Association,
         F.An_Array_Component_Association =>
           Asis.An_Array_Component_Association,
         F.A_Parameter_Association => Asis.A_Parameter_Association,
         F.A_Generic_Association => Asis.A_Generic_Association);

      Kind : constant Asis.Extensions.Flat_Kinds.Element_Flat_Kind :=
        Asis.Extensions.Flat_Kinds.Flat_Kind (Association);
   begin
      if Kind in Map'Range then
         return Map (Kind);
      else
         return Not_An_Association;
      end if;
   end Association_Kind;

   --------------------
   -- Attribute_Kind --
   --------------------

   function Attribute_Kind
     (Expression : in Asis.Expression)
      return Asis.Attribute_Kinds
   is
      Map : constant array (F.An_Attribute_Reference)
        of Asis.Attribute_Kinds :=
          (F.An_Access_Attribute => Asis.An_Access_Attribute,
           F.An_Address_Attribute => Asis.An_Address_Attribute,
           F.An_Adjacent_Attribute => Asis.An_Adjacent_Attribute,
           F.An_Aft_Attribute => Asis.An_Aft_Attribute,
           F.An_Alignment_Attribute => Asis.An_Alignment_Attribute,
           F.A_Base_Attribute => Asis.A_Base_Attribute,
           F.A_Bit_Order_Attribute => Asis.A_Bit_Order_Attribute,
           F.A_Body_Version_Attribute => Asis.A_Body_Version_Attribute,
           F.A_Callable_Attribute => Asis.A_Callable_Attribute,
           F.A_Caller_Attribute => Asis.A_Caller_Attribute,
           F.A_Ceiling_Attribute => Asis.A_Ceiling_Attribute,
           F.A_Class_Attribute => Asis.A_Class_Attribute,
           F.A_Component_Size_Attribute => Asis.A_Component_Size_Attribute,
           F.A_Compose_Attribute => Asis.A_Compose_Attribute,
           F.A_Constrained_Attribute => Asis.A_Constrained_Attribute,
           F.A_Copy_Sign_Attribute => Asis.A_Copy_Sign_Attribute,
           F.A_Count_Attribute => Asis.A_Count_Attribute,
           F.A_Definite_Attribute => Asis.A_Definite_Attribute,
           F.A_Delta_Attribute => Asis.A_Delta_Attribute,
           F.A_Denorm_Attribute => Asis.A_Denorm_Attribute,
           F.A_Digits_Attribute => Asis.A_Digits_Attribute,
           F.An_Exponent_Attribute => Asis.An_Exponent_Attribute,
           F.An_External_Tag_Attribute => Asis.An_External_Tag_Attribute,
           F.A_First_Attribute => Asis.A_First_Attribute,
           F.A_First_Bit_Attribute => Asis.A_First_Bit_Attribute,
           F.A_Floor_Attribute => Asis.A_Floor_Attribute,
           F.A_Fore_Attribute => Asis.A_Fore_Attribute,
           F.A_Fraction_Attribute => Asis.A_Fraction_Attribute,
           F.An_Identity_Attribute => Asis.An_Identity_Attribute,
           F.An_Image_Attribute => Asis.An_Image_Attribute,
           F.An_Input_Attribute => Asis.An_Input_Attribute,
           F.A_Last_Attribute => Asis.A_Last_Attribute,
           F.A_Last_Bit_Attribute => Asis.A_Last_Bit_Attribute,
           F.A_Leading_Part_Attribute => Asis.A_Leading_Part_Attribute,
           F.A_Length_Attribute => Asis.A_Length_Attribute,
           F.A_Machine_Attribute => Asis.A_Machine_Attribute,
           F.A_Machine_Emax_Attribute => Asis.A_Machine_Emax_Attribute,
           F.A_Machine_Emin_Attribute => Asis.A_Machine_Emin_Attribute,
           F.A_Machine_Mantissa_Attribute => Asis.A_Machine_Mantissa_Attribute,
           F.A_Machine_Overflows_Attribute =>
             Asis.A_Machine_Overflows_Attribute,
           F.A_Machine_Radix_Attribute => Asis.A_Machine_Radix_Attribute,
           F.A_Machine_Rounding_Attribute => Asis.A_Machine_Rounding_Attribute,
           F.A_Machine_Rounds_Attribute => Asis.A_Machine_Rounds_Attribute,
           F.A_Max_Attribute => Asis.A_Max_Attribute,
           F.A_Max_Size_In_Storage_Elements_Attribute =>
             Asis.A_Max_Size_In_Storage_Elements_Attribute,
           F.A_Min_Attribute => Asis.A_Min_Attribute,
           F.A_Mod_Attribute => Asis.A_Mod_Attribute,
           F.A_Model_Attribute => Asis.A_Model_Attribute,
           F.A_Model_Emin_Attribute => Asis.A_Model_Emin_Attribute,
           F.A_Model_Epsilon_Attribute => Asis.A_Model_Epsilon_Attribute,
           F.A_Model_Mantissa_Attribute => Asis.A_Model_Mantissa_Attribute,
           F.A_Model_Small_Attribute => Asis.A_Model_Small_Attribute,
           F.A_Modulus_Attribute => Asis.A_Modulus_Attribute,
           F.An_Output_Attribute => Asis.An_Output_Attribute,
           F.A_Partition_ID_Attribute => Asis.A_Partition_ID_Attribute,
           F.A_Pos_Attribute => Asis.A_Pos_Attribute,
           F.A_Position_Attribute => Asis.A_Position_Attribute,
           F.A_Pred_Attribute => Asis.A_Pred_Attribute,
           F.A_Priority_Attribute => Asis.A_Priority_Attribute,
           F.A_Range_Attribute => Asis.A_Range_Attribute,
           F.A_Read_Attribute => Asis.A_Read_Attribute,
           F.A_Remainder_Attribute => Asis.A_Remainder_Attribute,
           F.A_Round_Attribute => Asis.A_Round_Attribute,
           F.A_Rounding_Attribute => Asis.A_Rounding_Attribute,
           F.A_Safe_First_Attribute => Asis.A_Safe_First_Attribute,
           F.A_Safe_Last_Attribute => Asis.A_Safe_Last_Attribute,
           F.A_Scale_Attribute => Asis.A_Scale_Attribute,
           F.A_Scaling_Attribute => Asis.A_Scaling_Attribute,
           F.A_Signed_Zeros_Attribute => Asis.A_Signed_Zeros_Attribute,
           F.A_Size_Attribute => Asis.A_Size_Attribute,
           F.A_Small_Attribute => Asis.A_Small_Attribute,
           F.A_Storage_Pool_Attribute => Asis.A_Storage_Pool_Attribute,
           F.A_Storage_Size_Attribute => Asis.A_Storage_Size_Attribute,
           F.A_Stream_Size_Attribute => Asis.A_Stream_Size_Attribute,
           F.A_Succ_Attribute => Asis.A_Succ_Attribute,
           F.A_Tag_Attribute => Asis.A_Tag_Attribute,
           F.A_Terminated_Attribute => Asis.A_Terminated_Attribute,
           F.A_Truncation_Attribute => Asis.A_Truncation_Attribute,
           F.An_Unbiased_Rounding_Attribute =>
             Asis.An_Unbiased_Rounding_Attribute,
           F.An_Unchecked_Access_Attribute =>
             Asis.An_Unchecked_Access_Attribute,
           F.A_Val_Attribute => Asis.A_Val_Attribute,
           F.A_Valid_Attribute => Asis.A_Valid_Attribute,
           F.A_Value_Attribute => Asis.A_Value_Attribute,
           F.A_Version_Attribute => Asis.A_Version_Attribute,
           F.A_Wide_Image_Attribute => Asis.A_Wide_Image_Attribute,
           F.A_Wide_Value_Attribute => Asis.A_Wide_Value_Attribute,
           F.A_Wide_Wide_Image_Attribute => Asis.A_Wide_Wide_Image_Attribute,
           F.A_Wide_Wide_Value_Attribute => Asis.A_Wide_Wide_Value_Attribute,
           F.A_Wide_Wide_Width_Attribute => Asis.A_Wide_Wide_Width_Attribute,
           F.A_Wide_Width_Attribute => Asis.A_Wide_Width_Attribute,
           F.A_Width_Attribute => Asis.A_Width_Attribute,
           F.A_Write_Attribute => Asis.A_Write_Attribute,
           F.An_Implementation_Defined_Attribute =>
             Asis.An_Implementation_Defined_Attribute,
           F.An_Unknown_Attribute => Asis.An_Unknown_Attribute);

      Kind : constant Asis.Extensions.Flat_Kinds.Element_Flat_Kind :=
        Asis.Extensions.Flat_Kinds.Flat_Kind (Expression);
   begin
      if Kind in Map'Range then
         return Map (Kind);
      else
            return Not_An_Attribute;
      end if;
   end Attribute_Kind;

   -----------------
   -- Clause_Kind --
   -----------------

   function Clause_Kind (Clause : in Asis.Clause) return Asis.Clause_Kinds is
   begin
      case F.Flat_Kind (Clause) is
         when F.A_Use_Package_Clause =>
            return A_Use_Package_Clause;
         when F.A_Use_Type_Clause =>
            return A_Use_Type_Clause;
         when F.A_With_Clause =>
            return A_With_Clause;
         when F.A_Representation_Clause =>
            return A_Representation_Clause;
         when F.A_Component_Clause =>
            return A_Component_Clause;
         when others =>
            return Not_A_Clause;
      end case;
   end Clause_Kind;

   -------------------------
   -- Compilation_Pragmas --
   -------------------------

   function Compilation_Pragmas
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Asis.Pragma_Element_List
   is
   begin
      Check_Nil_Unit (Compilation_Unit, "Compilation_Pragmas");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Compilation_Pragmas;

   ---------------------------
   -- Configuration_Pragmas --
   ---------------------------

   function Configuration_Pragmas
     (The_Context : in Asis.Context)
      return Asis.Pragma_Element_List
   is
      pragma Unreferenced (The_Context);
   begin
      --  Check_Context (The_Context);
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Configuration_Pragmas;

   ---------------------
   -- Constraint_Kind --
   ---------------------

   function Constraint_Kind
     (Definition : in Asis.Constraint)
      return Asis.Constraint_Kinds
   is
      Map : constant array (F.A_Constraint) of Asis.Constraint_Kinds :=
        (F.A_Range_Attribute_Reference => Asis.A_Range_Attribute_Reference,
         F.A_Simple_Expression_Range => Asis.A_Simple_Expression_Range,
         F.A_Digits_Constraint => Asis.A_Digits_Constraint,
         F.A_Delta_Constraint => Asis.A_Delta_Constraint,
         F.An_Index_Constraint => Asis.An_Index_Constraint,
         F.A_Discriminant_Constraint => Asis.A_Discriminant_Constraint);

      Kind : constant Asis.Extensions.Flat_Kinds.Element_Flat_Kind :=
        Asis.Extensions.Flat_Kinds.Flat_Kind (Definition);
   begin
      if Kind in Map'Range then
         return Map (Kind);
      else
         return Not_A_Constraint;
      end if;
   end Constraint_Kind;

   -----------------------------
   -- Context_Clause_Elements --
   -----------------------------

   function Context_Clause_Elements
     (Compilation_Unit : in Asis.Compilation_Unit;
      Include_Pragmas  : in Boolean := False)
      return Asis.Context_Clause_List
   is
      pragma Unreferenced (Include_Pragmas);

      package Get is

         type Visiter is new Gela.Element_Visiters.Visiter with record
            Result : Gela.Elements.Context_Items.Context_Item_Sequence_Access;
         end record;

         overriding procedure Compilation_Unit_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Bodies.
              Compilation_Unit_Body_Access);

         overriding procedure Compilation_Unit_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Declarations.
              Compilation_Unit_Declaration_Access);

         overriding procedure Subunit
           (Self : in out Visiter;
            Node : not null Gela.Elements.Subunits.Subunit_Access);

      end Get;

      package body Get is

         overriding procedure Compilation_Unit_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Bodies.
              Compilation_Unit_Body_Access) is
         begin
            Self.Result := Node.Context_Clause_Elements;
         end Compilation_Unit_Body;

         overriding procedure Compilation_Unit_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Declarations.
              Compilation_Unit_Declaration_Access) is
         begin
            Self.Result := Node.Context_Clause_Elements;
         end Compilation_Unit_Declaration;

         overriding procedure Subunit
           (Self : in out Visiter;
            Node : not null Gela.Elements.Subunits.Subunit_Access) is
         begin
            Self.Result := Node.Context_Clause_Elements;
         end Subunit;

      end Get;

      Tree : Gela.Elements.Compilation_Units.Compilation_Unit_Access;
      V : aliased Get.Visiter;
   begin
      Check_Nil_Unit (Compilation_Unit, "Context_Clause_Elements");
      Tree := Compilation_Unit.Data.Tree;
      Tree.Visit (V);

      return Asis.To_List (Gela.Elements.Element_Sequence_Access (V.Result));
   end Context_Clause_Elements;

   ---------------------------
   -- Corresponding_Pragmas --
   ---------------------------

   function Corresponding_Pragmas
     (Element : in Asis.Element)
      return Asis.Pragma_Element_List
   is
   begin
      Check_Nil_Element (Element, "Corresponding_Pragmas");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Corresponding_Pragmas;

   -----------------
   -- Debug_Image --
   -----------------

   function Debug_Image (Element : in Asis.Element) return Wide_String is
      pragma Unreferenced (Element);
   begin
      return "";
   end Debug_Image;

   ----------------------
   -- Declaration_Kind --
   ----------------------

   function Declaration_Kind
     (Declaration : in Asis.Declaration)
      return Asis.Declaration_Kinds
   is
      Map : constant array (F.A_Declaration) of Asis.Declaration_Kinds :=
        (F.An_Ordinary_Type_Declaration => Asis.An_Ordinary_Type_Declaration,
         F.A_Task_Type_Declaration => Asis.A_Task_Type_Declaration,
         F.A_Protected_Type_Declaration => Asis.A_Protected_Type_Declaration,
         F.An_Incomplete_Type_Declaration =>
           Asis.An_Incomplete_Type_Declaration,
         F.A_Private_Type_Declaration => Asis.A_Private_Type_Declaration,
         F.A_Private_Extension_Declaration =>
           Asis.A_Private_Extension_Declaration,
         F.A_Subtype_Declaration => Asis.A_Subtype_Declaration,
         F.A_Variable_Declaration => Asis.A_Variable_Declaration,
         F.A_Constant_Declaration => Asis.A_Constant_Declaration,
         F.A_Deferred_Constant_Declaration =>
           Asis.A_Deferred_Constant_Declaration,
         F.A_Single_Task_Declaration => Asis.A_Single_Task_Declaration,
         F.A_Single_Protected_Declaration =>
           Asis.A_Single_Protected_Declaration,
         F.An_Integer_Number_Declaration => Asis.An_Integer_Number_Declaration,
         F.A_Real_Number_Declaration => Asis.A_Real_Number_Declaration,
         F.An_Enumeration_Literal_Specification =>
           Asis.An_Enumeration_Literal_Specification,
         F.A_Discriminant_Specification => Asis.A_Discriminant_Specification,
         F.A_Component_Declaration => Asis.A_Component_Declaration,
         F.A_Return_Object_Specification => Asis.A_Return_Object_Specification,
         F.A_Loop_Parameter_Specification =>
           Asis.A_Loop_Parameter_Specification,
         F.A_Procedure_Declaration => Asis.A_Procedure_Declaration,
         F.A_Function_Declaration => Asis.A_Function_Declaration,
         F.A_Parameter_Specification => Asis.A_Parameter_Specification,
         F.A_Procedure_Body_Declaration => Asis.A_Procedure_Body_Declaration,
         F.A_Function_Body_Declaration => Asis.A_Function_Body_Declaration,
         F.A_Package_Declaration => Asis.A_Package_Declaration,
         F.A_Package_Body_Declaration => Asis.A_Package_Body_Declaration,
         F.An_Object_Renaming_Declaration =>
           Asis.An_Object_Renaming_Declaration,
         F.An_Exception_Renaming_Declaration =>
           Asis.An_Exception_Renaming_Declaration,
         F.A_Package_Renaming_Declaration =>
           Asis.A_Package_Renaming_Declaration,
         F.A_Procedure_Renaming_Declaration =>
           Asis.A_Procedure_Renaming_Declaration,
         F.A_Function_Renaming_Declaration =>
           Asis.A_Function_Renaming_Declaration,
         F.A_Generic_Package_Renaming_Declaration =>
           Asis.A_Generic_Package_Renaming_Declaration,
         F.A_Generic_Procedure_Renaming_Declaration =>
           Asis.A_Generic_Procedure_Renaming_Declaration,
         F.A_Generic_Function_Renaming_Declaration =>
           Asis.A_Generic_Function_Renaming_Declaration,
         F.A_Task_Body_Declaration => Asis.A_Task_Body_Declaration,
         F.A_Protected_Body_Declaration => Asis.A_Protected_Body_Declaration,
         F.An_Entry_Declaration => Asis.An_Entry_Declaration,
         F.An_Entry_Body_Declaration => Asis.An_Entry_Body_Declaration,
         F.An_Entry_Index_Specification => Asis.An_Entry_Index_Specification,
         F.A_Procedure_Body_Stub => Asis.A_Procedure_Body_Stub,
         F.A_Function_Body_Stub => Asis.A_Function_Body_Stub,
         F.A_Package_Body_Stub => Asis.A_Package_Body_Stub,
         F.A_Task_Body_Stub => Asis.A_Task_Body_Stub,
         F.A_Protected_Body_Stub => Asis.A_Protected_Body_Stub,
         F.An_Exception_Declaration => Asis.An_Exception_Declaration,
         F.A_Choice_Parameter_Specification =>
           Asis.A_Choice_Parameter_Specification,
         F.A_Generic_Procedure_Declaration =>
           Asis.A_Generic_Procedure_Declaration,
         F.A_Generic_Function_Declaration =>
           Asis.A_Generic_Function_Declaration,
         F.A_Generic_Package_Declaration => Asis.A_Generic_Package_Declaration,
         F.A_Package_Instantiation => Asis.A_Package_Instantiation,
         F.A_Procedure_Instantiation => Asis.A_Procedure_Instantiation,
         F.A_Function_Instantiation => Asis.A_Function_Instantiation,
         F.A_Formal_Object_Declaration => Asis.A_Formal_Object_Declaration,
         F.A_Formal_Type_Declaration => Asis.A_Formal_Type_Declaration,
         F.A_Formal_Procedure_Declaration =>
           Asis.A_Formal_Procedure_Declaration,
         F.A_Formal_Function_Declaration => Asis.A_Formal_Function_Declaration,
         F.A_Formal_Package_Declaration => Asis.A_Formal_Package_Declaration,
         F.A_Formal_Package_Declaration_With_Box =>
           Asis.A_Formal_Package_Declaration_With_Box);

      Kind : constant Asis.Extensions.Flat_Kinds.Element_Flat_Kind :=
        Asis.Extensions.Flat_Kinds.Flat_Kind (Declaration);
   begin
      if Kind in Map'Range then
         return Map (Kind);
      else
         return Not_A_Declaration;
      end if;
   end Declaration_Kind;

   ------------------------
   -- Declaration_Origin --
   ------------------------

   function Declaration_Origin
     (Declaration : in Asis.Declaration)
      return Asis.Declaration_Origins
   is
   begin
      if Assigned (Declaration) then
         Raise_Not_Implemented ("");
         return Not_A_Declaration_Origin;
      else
         return Not_A_Declaration_Origin;
      end if;
   end Declaration_Origin;

   ------------------
   -- Default_Kind --
   ------------------

   function Default_Kind
     (Declaration : in Asis.Generic_Formal_Parameter)
      return Asis.Subprogram_Default_Kinds
   is
   begin
      if Assigned (Declaration) then
         Raise_Not_Implemented ("");
         return Not_A_Default;
      else
         return Not_A_Default;
      end if;
   end Default_Kind;

   ------------------------
   -- Defining_Name_Kind --
   ------------------------

   function Defining_Name_Kind
     (Defining_Name : in Asis.Defining_Name)
      return Asis.Defining_Name_Kinds
   is
   begin
      case F.Flat_Kind (Defining_Name) is
         when F.A_Defining_Identifier =>
            return Asis.A_Defining_Identifier;
         when F.A_Defining_Character_Literal =>
            return Asis.A_Defining_Character_Literal;
         when F.A_Defining_Enumeration_Literal =>
            return Asis.A_Defining_Enumeration_Literal;
         when F.A_Defining_Operator_Symbol =>
            return Asis.A_Defining_Operator_Symbol;
         when F.A_Defining_Expanded_Name =>
            return Asis.A_Defining_Expanded_Name;
         when others =>
            return Not_A_Defining_Name;
      end case;
   end Defining_Name_Kind;

   ---------------------
   -- Definition_Kind --
   ---------------------

   function Definition_Kind
     (Definition : in Asis.Definition)
      return Asis.Definition_Kinds
   is
   begin
      case F.Flat_Kind (Definition) is
         when F.A_Type_Definition =>
            return Asis.A_Type_Definition;
         when F.A_Subtype_Indication =>
            return Asis.A_Subtype_Indication;
         when F.A_Constraint =>
            return Asis.A_Constraint;
         when F.A_Component_Definition =>
            return Asis.A_Component_Definition;
         when F.A_Discrete_Subtype_Definition =>
            return Asis.A_Discrete_Subtype_Definition;
         when F.A_Discrete_Range =>
            return Asis.A_Discrete_Range;
         when F.An_Unknown_Discriminant_Part =>
            return Asis.An_Unknown_Discriminant_Part;
         when F.A_Known_Discriminant_Part =>
            return Asis.A_Known_Discriminant_Part;
         when F.A_Record_Definition =>
            return Asis.A_Record_Definition;
         when F.A_Null_Record_Definition =>
            return Asis.A_Null_Record_Definition;
         when F.A_Null_Component =>
            return Asis.A_Null_Component;
         when F.A_Variant_Part =>
            return Asis.A_Variant_Part;
         when F.A_Variant =>
            return Asis.A_Variant;
         when F.An_Others_Choice =>
            return Asis.An_Others_Choice;
         when F.An_Access_Definition =>
            return Asis.An_Access_Definition;
         when F.An_Incomplete_Type_Definition =>
            return Asis.An_Incomplete_Type_Definition;
         when F.A_Tagged_Incomplete_Type_Definition =>
            return Asis.A_Tagged_Incomplete_Type_Definition;
         when F.A_Private_Type_Definition =>
            return Asis.A_Private_Type_Definition;
         when F.A_Tagged_Private_Type_Definition =>
            return Asis.A_Tagged_Private_Type_Definition;
         when F.A_Private_Extension_Definition =>
            return Asis.A_Private_Extension_Definition;
         when F.A_Task_Definition =>
            return Asis.A_Task_Definition;
         when F.A_Protected_Definition =>
            return Asis.A_Protected_Definition;
         when F.A_Formal_Type_Definition =>
            return Asis.A_Formal_Type_Definition;
         when others =>
            return Asis.Not_A_Definition;
      end case;
   end Definition_Kind;

   -------------------------
   -- Discrete_Range_Kind --
   -------------------------

   function Discrete_Range_Kind
     (Definition : in Asis.Discrete_Range)
      return Asis.Discrete_Range_Kinds
   is
      Map : constant array (F.A_Discrete_Range) of Asis.Discrete_Range_Kinds
        := (F.A_Discrete_Subtype_Indication_DR =>
              A_Discrete_Subtype_Indication,
            F.A_Discrete_Range_Attribute_Reference_DR =>
              A_Discrete_Range_Attribute_Reference,
            F.A_Discrete_Simple_Expression_Range_DR =>
              A_Discrete_Simple_Expression_Range);

      Map_2 : constant array (F.A_Discrete_Subtype_Definition)
        of Asis.Discrete_Range_Kinds
        := (F.A_Discrete_Subtype_Indication =>
              A_Discrete_Subtype_Indication,
            F.A_Discrete_Range_Attribute_Reference =>
              A_Discrete_Range_Attribute_Reference,
            F.A_Discrete_Simple_Expression_Range =>
              A_Discrete_Simple_Expression_Range);

      Kind : constant Asis.Extensions.Flat_Kinds.Element_Flat_Kind :=
        Asis.Extensions.Flat_Kinds.Flat_Kind (Definition);
   begin
      if Kind in Map'Range then
         return Map (Kind);
      elsif Kind in Map_2'Range then
         return Map_2 (Kind);
      else
         return Not_A_Discrete_Range;
      end if;
   end Discrete_Range_Kind;

   ------------------
   -- Element_Kind --
   ------------------

   function Element_Kind
     (Element : in Asis.Element) return Asis.Element_Kinds is
   begin
      case F.Flat_Kind (Element) is
         when F.A_Pragma =>
            return A_Pragma;
         when F.A_Defining_Name =>
            return A_Defining_Name;
         when F.A_Declaration =>
            return A_Declaration;
         when F.A_Definition =>
            return A_Definition;
         when F.An_Expression =>
            return An_Expression;
         when F.An_Association =>
            return An_Association;
         when F.A_Statement =>
            return A_Statement;
         when F.A_Path =>
            return A_Path;
         when F.A_Clause =>
            return A_Clause;
         when F.An_Exception_Handler =>
            return An_Exception_Handler;
         when others =>
            return Not_An_Element;
      end case;
   end Element_Kind;

   --------------------------------
   -- Enclosing_Compilation_Unit --
   --------------------------------

   function Enclosing_Compilation_Unit
     (Element : in Asis.Element)
      return Asis.Compilation_Unit
   is
      use type Gela.Compilation_Units.Compilation_Unit_Access;

      procedure Find
        (List : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
         Unit : out Gela.Compilation_Units.Compilation_Unit_Access);

      Comp    : Gela.Compilations.Compilation_Access;
      From    : Gela.Lexical_Types.Token_Count;
      To      : Gela.Lexical_Types.Token_Count;

      ----------
      -- Find --
      ----------

      procedure Find
        (List : Gela.Compilation_Unit_Sets.Compilation_Unit_Set_Access;
         Unit : out Gela.Compilation_Units.Compilation_Unit_Access)
      is
         use Gela.Compilations;
         use type Gela.Lexical_Types.Token_Count;
         Tree    : Gela.Elements.Compilation_Units.Compilation_Unit_Access;
         Cursor  : Gela.Compilation_Unit_Sets.Compilation_Unit_Cursor'Class :=
           List.First;
      begin
         while Cursor.Has_Element loop
            Unit := Cursor.Element;
            Tree := Unit.Tree;

            if Unit.Compilation = Comp
              and then Tree.First_Token <= From
              and then Tree.Last_Token >= To
            then
               return;
            end if;

            Cursor.Next;
         end loop;

         Unit := null;
      end Find;

      Context : Gela.Contexts.Context_Access;
      Unit    : Gela.Compilation_Units.Compilation_Unit_Access;

   begin
      Check_Nil_Element (Element, "Enclosing_Compilation_Unit");
      Comp := Element.Data.Enclosing_Compilation;
      Context := Comp.Context;
      From := Element.Data.First_Token;
      To := Element.Data.Last_Token;
      Find (Context.Library_Unit_Declarations, Unit);

      if Unit = null then
         Find (Context.Compilation_Unit_Bodies, Unit);
      end if;

      --  Raise_Not_Implemented ("");
      return (Data => Unit);
   end Enclosing_Compilation_Unit;

   -----------------------
   -- Enclosing_Element --
   -----------------------

   function Enclosing_Element
     (Element : in Asis.Element)
      return Asis.Element
   is
      Next : Asis.Element := Element;
   begin
      Check_Nil_Element (Element, "Enclosing_Element");
      loop
         Next := (Data => Next.Data.Enclosing_Element);

         if not Assigned (Next) or else not Auxilary (Next) then
            return Next;
         end if;
      end loop;
   end Enclosing_Element;

   -----------------------
   -- Enclosing_Element --
   -----------------------

   function Enclosing_Element
     (Element                    : in Asis.Element;
      Expected_Enclosing_Element : in Asis.Element)
      return Asis.Element
   is
      pragma Unreferenced (Expected_Enclosing_Element);
   begin
      return Enclosing_Element (Element);
   end Enclosing_Element;

   ---------------------
   -- Expression_Kind --
   ---------------------

   function Expression_Kind
     (Expression : in Asis.Expression) return Asis.Expression_Kinds is
   begin
      case F.Flat_Kind (Expression) is
         when F.A_Box_Expression =>
            return Asis.A_Box_Expression;
         when F.An_Integer_Literal =>
            return Asis.An_Integer_Literal;
         when F.A_Real_Literal =>
            return Asis.A_Real_Literal;
         when F.A_String_Literal =>
            return Asis.A_String_Literal;
         when F.An_Identifier =>
            return Asis.An_Identifier;
         when F.An_Operator_Symbol =>
            return Asis.An_Operator_Symbol;
         when F.A_Character_Literal =>
            return Asis.A_Character_Literal;
         when F.An_Enumeration_Literal =>
            return Asis.An_Enumeration_Literal;
         when F.An_Explicit_Dereference =>
            return Asis.An_Explicit_Dereference;
         when F.A_Function_Call =>
            return Asis.A_Function_Call;
         when F.An_Indexed_Component =>
            return Asis.An_Indexed_Component;
         when F.A_Slice =>
            return Asis.A_Slice;
         when F.A_Selected_Component =>
            return Asis.A_Selected_Component;
         when F.An_Attribute_Reference =>
            return Asis.An_Attribute_Reference;
         when F.A_Record_Aggregate =>
            return Asis.A_Record_Aggregate;
         when F.An_Extension_Aggregate =>
            return Asis.An_Extension_Aggregate;
         when F.A_Positional_Array_Aggregate =>
            return Asis.A_Positional_Array_Aggregate;
         when F.A_Named_Array_Aggregate =>
            return Asis.A_Named_Array_Aggregate;
         when F.An_And_Then_Short_Circuit =>
            return Asis.An_And_Then_Short_Circuit;
         when F.An_Or_Else_Short_Circuit =>
            return Asis.An_Or_Else_Short_Circuit;
         when F.An_In_Membership_Test =>
            return Asis.An_In_Membership_Test;
         when F.A_Not_In_Membership_Test =>
            return Asis.A_Not_In_Membership_Test;
         when F.A_Null_Literal =>
            return Asis.A_Null_Literal;
         when F.A_Parenthesized_Expression =>
            return Asis.A_Parenthesized_Expression;
         when F.A_Type_Conversion =>
            return Asis.A_Type_Conversion;
         when F.A_Qualified_Expression =>
            return Asis.A_Qualified_Expression;
         when F.An_Allocation_From_Subtype =>
            return Asis.An_Allocation_From_Subtype;
         when F.An_Allocation_From_Qualified_Expression =>
            return Asis.An_Allocation_From_Qualified_Expression;
         when others =>
            return Not_An_Expression;
      end case;
   end Expression_Kind;

   ----------------------
   -- Formal_Type_Kind --
   ----------------------

   function Formal_Type_Kind
     (Definition : in Asis.Formal_Type_Definition)
      return Asis.Formal_Type_Kinds
   is
   begin
      if Assigned (Definition) then
         Raise_Not_Implemented ("");
         return Not_A_Formal_Type_Definition;
      else
         return Not_A_Formal_Type_Definition;
      end if;
   end Formal_Type_Kind;

   -----------------
   -- Has_Limited --
   -----------------

   function Has_Limited (Element : in Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Has_Limited;

   -----------------
   -- Has_Private --
   -----------------

   function Has_Private (Element : in Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Has_Private;

   ------------------
   -- Has_Abstract --
   ------------------

   function Has_Abstract (Element : in Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Has_Abstract;

   -----------------
   -- Has_Reverse --
   -----------------

   function Has_Reverse (Element : in Asis.Element) return Boolean is
   begin
      return Trait_Kind (Element) = A_Reverse_Trait;
   end Has_Reverse;

   -----------------
   -- Has_Aliased --
   -----------------

   function Has_Aliased (Element : in Asis.Element) return Boolean is
   begin
      return Trait_Kind (Element) = An_Aliased_Trait;
   end Has_Aliased;

   ----------------------
   -- Has_Synchronized --
   ----------------------

   function Has_Synchronized (Element : in Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Has_Synchronized;

   -------------------
   -- Has_Protected --
   -------------------

   function Has_Protected (Element : in Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Has_Protected;

   ----------------
   -- Has_Tagged --
   ----------------

   function Has_Tagged (Element : in Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Has_Tagged;

   --------------
   -- Has_Task --
   --------------

   function Has_Task (Element : in Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Has_Task;

   ------------------------
   -- Has_Null_Exclusion --
   ------------------------

   function Has_Null_Exclusion (Element : Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Has_Null_Exclusion;

   ----------
   -- Hash --
   ----------

   function Hash (Element : in Asis.Element) return Asis.ASIS_Integer is
      use type Ada.Containers.Hash_Type;

      X : Ada.Containers.Hash_Type;
   begin
      if Assigned (Element) then
         X := Element.Data.Hash;
         X := X and Ada.Containers.Hash_Type (ASIS_Integer'Last);
         return ASIS_Integer (X);
      else
         return 0;
      end if;
   end Hash;

   --------------------
   -- Interface_Kind --
   --------------------

   function Interface_Kind
     (Definition : Asis.Definition)
     return Asis.Interface_Kinds is
   begin
      if Assigned (Definition) then
         if Type_Kind (Definition) = An_Interface_Type_Definition or
           Formal_Type_Kind (Definition) =
           A_Formal_Interface_Type_Definition
         then
            if Has_Task (Definition) then
               return A_Task_Interface;
            elsif Has_Limited (Definition) then
               return A_Limited_Interface;
            elsif Has_Protected (Definition) then
               return A_Protected_Interface;
            elsif Has_Synchronized (Definition) then
               return A_Synchronized_Interface;
            else
               return An_Ordinary_Interface;
            end if;
         end if;
      end if;

      return Not_An_Interface;
   end Interface_Kind;

   ----------------------------
   -- Is_Abstract_Subprogram --
   ----------------------------

   function Is_Abstract_Subprogram
     (Element : in Asis.Element)
      return Boolean
   is
   begin
      case Declaration_Kind (Element) is
         when A_Procedure_Declaration |
           A_Function_Declaration |
           A_Formal_Procedure_Declaration |
           A_Formal_Function_Declaration =>
            return Has_Abstract (Element);
         when others =>
            return False;
      end case;
   end Is_Abstract_Subprogram;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal
     (Left  : in Asis.Element;
      Right : in Asis.Element)
      return Boolean
   is
      pragma Unreferenced (Left);
      pragma Unreferenced (Right);
   begin
      Raise_Not_Implemented ("");
      return False;
   end Is_Equal;

   ------------------
   -- Is_Identical --
   ------------------

   function Is_Identical
     (Left  : in Asis.Element;
      Right : in Asis.Element)
      return Boolean
   is
      use type Gela.Elements.Element_Access;
   begin
      return Left.Data = Right.Data;
   end Is_Identical;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Right : in Asis.Element) return Boolean is
   begin
      return not Assigned (Right);
   end Is_Nil;

   ------------
   -- Is_Nil --
   ------------

   function Is_Nil (Right : in Asis.Element_List) return Boolean is
   begin
      return Right'Length = 0;
   end Is_Nil;

   -----------------------
   -- Is_Null_Procedure --
   -----------------------

   function Is_Null_Procedure (Element : in Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return False;
      else
         return False;
      end if;
   end Is_Null_Procedure;

   -------------------------
   -- Is_Part_Of_Implicit --
   -------------------------

   function Is_Part_Of_Implicit (Element : in Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         return Element.Data.Is_Part_Of_Implicit;
      else
         return False;
      end if;
   end Is_Part_Of_Implicit;

   --------------------------
   -- Is_Part_Of_Inherited --
   --------------------------

   function Is_Part_Of_Inherited
     (Element : in Asis.Element)
      return Boolean
   is
   begin
      if Assigned (Element) then
         return Element.Data.Is_Part_Of_Inherited;
      else
         return False;
      end if;
   end Is_Part_Of_Inherited;

   -------------------------
   -- Is_Part_Of_Instance --
   -------------------------

   function Is_Part_Of_Instance (Element : in Asis.Element) return Boolean is
   begin
      if Assigned (Element) then
         return Element.Data.Is_Part_Of_Instance;
      else
         return False;
      end if;
   end Is_Part_Of_Instance;

   ---------------
   -- Mode_Kind --
   ---------------

   function Mode_Kind
     (Declaration : in Asis.Declaration)
      return Asis.Mode_Kinds
   is
   begin
      if Assigned (Declaration) then
         Raise_Not_Implemented ("");
         return Not_A_Mode;
      else
         return Not_A_Mode;
      end if;
   end Mode_Kind;

   -------------------
   -- Operator_Kind --
   -------------------

   function Operator_Kind
     (Element : in Asis.Element)
      return Asis.Operator_Kinds
   is
   begin
      if Assigned (Element) then
         Raise_Not_Implemented ("");
         return Not_An_Operator;
      else
         return Not_An_Operator;
      end if;
   end Operator_Kind;

   ---------------
   -- Path_Kind --
   ---------------

   function Path_Kind (Path : in Asis.Path) return Asis.Path_Kinds is
      Map : constant array (F.A_Path) of Asis.Path_Kinds
        := (F.An_If_Path => Asis.An_If_Path,
            F.An_Elsif_Path => Asis.An_Elsif_Path,
            F.An_Else_Path => Asis.An_Else_Path,
            F.A_Case_Path => Asis.A_Case_Path,
            F.A_Select_Path => Asis.A_Select_Path,
            F.An_Or_Path => Asis.An_Or_Path,
            F.A_Then_Abort_Path => Asis.A_Then_Abort_Path);

      Kind : constant Asis.Extensions.Flat_Kinds.Element_Flat_Kind :=
        Asis.Extensions.Flat_Kinds.Flat_Kind (Path);
   begin
      if Kind in Map'Range then
         return Map (Kind);
      else
         return Not_A_Path;
      end if;
   end Path_Kind;

   ----------------------------------
   -- Pragma_Argument_Associations --
   ----------------------------------

   function Pragma_Argument_Associations
     (Pragma_Element : in Asis.Pragma_Element)
      return Asis.Association_List
   is
   begin
      Check_Nil_Element (Pragma_Element, "Pragma_Argument_Associations");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Pragma_Argument_Associations;

   -----------------
   -- Pragma_Kind --
   -----------------

   function Pragma_Kind
     (Pragma_Element : in Asis.Pragma_Element)
      return Asis.Pragma_Kinds
   is
   begin
      if Assigned (Pragma_Element) then
         return Not_A_Pragma;
      else
         return Not_A_Pragma;
      end if;
   end Pragma_Kind;

   -----------------------
   -- Pragma_Name_Image --
   -----------------------

   function Pragma_Name_Image
     (Pragma_Element : in Asis.Pragma_Element)
      return Program_Text
   is
   begin
      Check_Nil_Element (Pragma_Element, "Pragma_Name_Image");
      Raise_Not_Implemented ("");
      return "";
   end Pragma_Name_Image;

   -------------
   -- Pragmas --
   -------------

   function Pragmas
     (The_Element : in Asis.Element)
      return Asis.Pragma_Element_List
   is
   begin
      Check_Nil_Element (The_Element, "Pragmas");
      Raise_Not_Implemented ("");
      return Asis.Nil_Element_List;
   end Pragmas;

   --------------------------------
   -- Representation_Clause_Kind --
   --------------------------------

   function Representation_Clause_Kind
     (Clause : in Asis.Representation_Clause)
      return Asis.Representation_Clause_Kinds
   is
   begin
      if Assigned (Clause) then
         Raise_Not_Implemented ("");
         return Not_A_Representation_Clause;
      else
         return Not_A_Representation_Clause;
      end if;
   end Representation_Clause_Kind;

   --------------------
   -- Root_Type_Kind --
   --------------------

   function Root_Type_Kind
     (Definition : in Asis.Root_Type_Definition)
      return Asis.Root_Type_Kinds
   is
   begin
      if Assigned (Definition) then
         Raise_Not_Implemented ("");
         return Not_A_Root_Type_Definition;
      else
         return Not_A_Root_Type_Definition;
      end if;
   end Root_Type_Kind;

   --------------------
   -- Statement_Kind --
   --------------------

   function Statement_Kind
     (Statement : in Asis.Statement)
      return Asis.Statement_Kinds
   is
      Map : constant array (F.A_Statement) of Asis.Statement_Kinds :=
        (F.A_Null_Statement => Asis.A_Null_Statement,
         F.An_Assignment_Statement => Asis.An_Assignment_Statement,
         F.An_If_Statement => Asis.An_If_Statement,
         F.A_Case_Statement => Asis.A_Case_Statement,
         F.A_Loop_Statement => Asis.A_Loop_Statement,
         F.A_While_Loop_Statement => Asis.A_While_Loop_Statement,
         F.A_For_Loop_Statement => Asis.A_For_Loop_Statement,
         F.A_Block_Statement => Asis.A_Block_Statement,
         F.An_Exit_Statement => Asis.An_Exit_Statement,
         F.A_Goto_Statement => Asis.A_Goto_Statement,
         F.A_Procedure_Call_Statement => Asis.A_Procedure_Call_Statement,
         F.A_Simple_Return_Statement => Asis.A_Simple_Return_Statement,
         F.An_Extended_Return_Statement => Asis.An_Extended_Return_Statement,
         F.An_Accept_Statement => Asis.An_Accept_Statement,
         F.An_Entry_Call_Statement => Asis.An_Entry_Call_Statement,
         F.A_Requeue_Statement => Asis.A_Requeue_Statement,
         F.A_Requeue_Statement_With_Abort =>
           Asis.A_Requeue_Statement_With_Abort,
         F.A_Delay_Until_Statement => Asis.A_Delay_Until_Statement,
         F.A_Delay_Relative_Statement => Asis.A_Delay_Relative_Statement,
         F.A_Terminate_Alternative_Statement =>
           Asis.A_Terminate_Alternative_Statement,
         F.A_Selective_Accept_Statement => Asis.A_Selective_Accept_Statement,
         F.A_Timed_Entry_Call_Statement => Asis.A_Timed_Entry_Call_Statement,
         F.A_Conditional_Entry_Call_Statement =>
           Asis.A_Conditional_Entry_Call_Statement,
         F.An_Asynchronous_Select_Statement =>
           Asis.An_Asynchronous_Select_Statement,
         F.An_Abort_Statement => Asis.An_Abort_Statement,
         F.A_Raise_Statement => Asis.A_Raise_Statement,
         F.A_Code_Statement => Asis.A_Code_Statement);

      Kind : constant Asis.Extensions.Flat_Kinds.Element_Flat_Kind :=
        Asis.Extensions.Flat_Kinds.Flat_Kind (Statement);
   begin
      if Kind in Map'Range then
         return Map (Kind);
      else
         return Not_A_Statement;
      end if;
   end Statement_Kind;

   ----------------
   -- Trait_Kind --
   ----------------

   function Trait_Kind
     (Element : in Asis.Element)
      return Asis.Trait_Kinds
   is
   begin
      if Assigned (Element) then
--         Raise_Not_Implemented ("");
         return Not_A_Trait;
      else
         return Not_A_Trait;
      end if;
   end Trait_Kind;

   ---------------
   -- Type_Kind --
   ---------------

   function Type_Kind
     (Definition : in Asis.Type_Definition)
      return Asis.Type_Kinds
   is
   begin
      case F.Flat_Kind (Definition) is
         when F.A_Derived_Type_Definition =>
            return Asis.A_Derived_Type_Definition;
         when F.A_Derived_Record_Extension_Definition =>
            return Asis.A_Derived_Record_Extension_Definition;
         when F.An_Enumeration_Type_Definition =>
            return Asis.An_Enumeration_Type_Definition;
         when F.A_Signed_Integer_Type_Definition =>
            return Asis.A_Signed_Integer_Type_Definition;
         when F.A_Modular_Type_Definition =>
            return Asis.A_Modular_Type_Definition;
         when F.A_Root_Type_Definition =>
            return Asis.A_Root_Type_Definition;
         when F.A_Floating_Point_Definition =>
            return A_Floating_Point_Definition;
         when F.An_Ordinary_Fixed_Point_Definition =>
            return An_Ordinary_Fixed_Point_Definition;
         when F.A_Decimal_Fixed_Point_Definition =>
            return A_Decimal_Fixed_Point_Definition;
         when F.An_Unconstrained_Array_Definition =>
            return An_Unconstrained_Array_Definition;
         when F.A_Constrained_Array_Definition =>
            return A_Constrained_Array_Definition;
         when F.A_Record_Type_Definition =>
            return A_Record_Type_Definition;
         when F.A_Tagged_Record_Type_Definition =>
            return A_Tagged_Record_Type_Definition;
         when F.An_Interface_Type_Definition =>
            return Asis.An_Interface_Type_Definition;
         when F.An_Access_Type_Definition =>
            return Asis.An_Access_Type_Definition;
         when others =>
            return Not_A_Type_Definition;
      end case;
   end Type_Kind;

   ----------------------
   -- Unit_Declaration --
   ----------------------

   function Unit_Declaration
     (Compilation_Unit : in Asis.Compilation_Unit)
      return Asis.Declaration
   is
      package Get is

         type Visiter is new Gela.Element_Visiters.Visiter with record
            Unit : Gela.Elements.Element_Access;
         end record;

         overriding procedure Compilation_Unit_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Bodies.
              Compilation_Unit_Body_Access);

         overriding procedure Compilation_Unit_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Declarations.
              Compilation_Unit_Declaration_Access);

         overriding procedure Subunit
           (Self : in out Visiter;
            Node : not null Gela.Elements.Subunits.Subunit_Access);

      end Get;

      package body Get is

         overriding procedure Compilation_Unit_Body
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Bodies.
              Compilation_Unit_Body_Access)
         is
            Result : constant Gela.Elements.Library_Unit_Bodies.
              Library_Unit_Body_Access := Node.Unit_Declaration;
         begin
            Self.Unit := Gela.Elements.Element_Access (Result);
         end Compilation_Unit_Body;

         overriding procedure Compilation_Unit_Declaration
           (Self : in out Visiter;
            Node : not null Gela.Elements.Compilation_Unit_Declarations.
              Compilation_Unit_Declaration_Access)
         is
            Result : constant Gela.Elements.Library_Unit_Declarations.
              Library_Unit_Declaration_Access := Node.Unit_Declaration;
         begin
            Self.Unit := Gela.Elements.Element_Access (Result);
         end Compilation_Unit_Declaration;

         overriding procedure Subunit
           (Self : in out Visiter;
            Node : not null Gela.Elements.Subunits.Subunit_Access)
         is
            Result : constant Gela.Elements.Proper_Bodies.Proper_Body_Access :=
              Node.Unit_Declaration;
         begin
            Self.Unit := Gela.Elements.Element_Access (Result);
         end Subunit;

      end Get;

      Tree : Gela.Elements.Compilation_Units.Compilation_Unit_Access;
      V    : aliased Get.Visiter;
   begin
      Check_Nil_Unit (Compilation_Unit, "Unit_Declaration");
      Tree := Compilation_Unit.Data.Tree;
      Tree.Visit (V);

      return (Data => V.Unit);
   end Unit_Declaration;

end Asis.Elements;


------------------------------------------------------------------------------
--  Copyright (c) 2006-2013, Maxim Reznik
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the Maxim Reznik, IE nor the names of its
--       contributors may be used to endorse or promote products derived from
--       this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
------------------------------------------------------------------------------
