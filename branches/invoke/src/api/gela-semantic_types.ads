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

end Gela.Semantic_Types;
