--  This package provides types related to semantic information.

package Gela.Semantic_Types is
   pragma Preelaborate;

   type Unit_Kinds is
     (A_Package_Body,
      A_Function_Body,
      A_Procedure_Body,
      A_Procedure,
      A_Function,
      A_Package,
      A_Generic_Procedure,
      A_Generic_Function,
      A_Generic_Package,
      A_Package_Instance,
      A_Procedure_Instance,
      A_Function_Instance,
      A_Package_Renaming,
      A_Generic_Package_Renaming,
      A_Generic_Procedure_Renaming,
      A_Generic_Function_Renaming);

   type Env_Index is new Natural;
   --  Index to reference an instance of environment state

   type Error_Set_Index is new Natural;
   --  Index to reference a set of errors

   type Type_View_Index is new Natural;
   --  Index of type view

   type Type_Index_Array is array (Positive range <>) of Type_View_Index;
   --  Array of type view indexes

   type Value_Index is new Natural;
   --  Index of static value

   --  See also Gela.Lexical_Types.Operators
   type Static_Operator is
     (Less_Operator,
      Equal_Operator,
      Greater_Operator,
      Hyphen_Operator,
      Slash_Operator,
      Star_Operator,
      Ampersand_Operator,
      Plus_Operator,
      Less_Or_Equal_Operator,
      Greater_Or_Equal_Operator,
      Inequality_Operator,
      Double_Star_Operator,
      Or_Operator,
      And_Operator,
      Xor_Operator,
      Mod_Operator,
      Rem_Operator,
      Abs_Operator,
      Not_Operator);

end Gela.Semantic_Types;
