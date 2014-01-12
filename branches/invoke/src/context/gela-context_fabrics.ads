with League.String_Vectors;
with Gela.Contexts;

package Gela.Context_Fabrics is
   pragma Preelaborate;

   function Create_Context
     (Parameters : League.String_Vectors.Universal_String_Vector)
      return Gela.Contexts.Context_Access;

end Gela.Context_Fabrics;
