with Gela.Compilations;
with Gela.Elements.Package_Instantiations;
with Gela.Semantic_Types;

package Gela.Instantiation is
   pragma Preelaborate;

   --  Each instantination declaration has property Expanded with
   --  deep copy of corresponding generic declaration.
   --
   --  Defining_Name property of each identifier in this copy is fixed to refer
   --  to copied defining name. Each defining_name in the copy has property
   --  named Corresponding_Generic_Element. It refers to corresponding
   --  defining name in the template.
   --
   --  Each formal declaration of this copy has property Corresponding_View.
   --  It refers to corresponding expression of Generic_Actual_Part.
   --  Type manager takes this property into account when resolves types in
   --  Generic_Actual_Part and in other places outside of instance.

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
