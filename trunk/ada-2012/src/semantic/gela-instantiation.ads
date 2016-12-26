with Gela.Compilations;
with Gela.Elements.Package_Instantiations;

package Gela.Instantiation is
   pragma Preelaborate;

   procedure Expand
     (Comp         : Gela.Compilations.Compilation_Access;
      Node         : not null Gela.Elements.Package_Instantiations.
        Package_Instantiation_Access;
      Expanded     : out Gela.Elements.Element_Access);

end Gela.Instantiation;
