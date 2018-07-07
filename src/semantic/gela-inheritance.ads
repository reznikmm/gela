with Gela.Compilations;
with Gela.Elements.Derived_Type_Definitions;
with Gela.Semantic_Types;

package Gela.Inheritance is
   pragma Preelaborate;

   procedure Copy_Declarations
     (Comp         : Gela.Compilations.Compilation_Access;
      Env          : Gela.Semantic_Types.Env_Index;
      Node         : not null Gela.Elements.Derived_Type_Definitions.
        Derived_Type_Definition_Access;
      Inherited    : out Gela.Elements.Element_Sequence_Access);

   procedure Environment
     (Comp         : Gela.Compilations.Compilation_Access;
      Node         : not null Gela.Elements.Derived_Type_Definitions.
        Derived_Type_Definition_Access;
      Env_In       : Gela.Semantic_Types.Env_Index;
      Env_Out      : out Gela.Semantic_Types.Env_Index);

end Gela.Inheritance;
