with League.String_Vectors;
with League.Strings;
with Gela.Contexts;

package Gela.Context_Factories is
   pragma Preelaborate;

   function Create_Context
     (Parameters : League.String_Vectors.Universal_String_Vector;
      Include    : League.Strings.Universal_String)
      return Gela.Contexts.Context_Access;
   --  Parameters list of parameters:
   --  -I<PATH> - include path
   --  --debug=PROP_LIST, where PROP_LIST list separated by comma of
   --     UP         -  print set of possible interpretations
   --     DOWN       -  print chosen interpretation
   --     ENV_IN     -  print input environment
   --     ENV_OUT    -  print output environment
   --     FULL_NAME  -  print symbol

end Gela.Context_Factories;
