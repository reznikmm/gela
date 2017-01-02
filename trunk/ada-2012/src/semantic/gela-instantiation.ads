with Gela.Compilations;
with Gela.Elements.Package_Instantiations;
with Gela.Semantic_Types;

package Gela.Instantiation is
   pragma Preelaborate;

   procedure Expand
     (Comp         : Gela.Compilations.Compilation_Access;
      Node         : not null Gela.Elements.Package_Instantiations.
        Package_Instantiation_Access;
      Expanded     : out Gela.Elements.Element_Access);

   procedure Environment
     (Comp         : Gela.Compilations.Compilation_Access;
      Node         : not null Gela.Elements.Package_Instantiations.
        Package_Instantiation_Access;
      Env_In       : Gela.Semantic_Types.Env_Index;
      Env_Out      : out Gela.Semantic_Types.Env_Index);

end Gela.Instantiation;
