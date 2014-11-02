with League.String_Vectors;
with League.Strings;
with Gela.Contexts;

package Gela.Context_Factories is
   pragma Preelaborate;

   function Create_Context
     (Parameters : League.String_Vectors.Universal_String_Vector;
      Include    : League.Strings.Universal_String)
      return Gela.Contexts.Context_Access;

end Gela.Context_Factories;
