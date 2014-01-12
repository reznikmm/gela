with Gela.Plain_Contexts;

package body Gela.Context_Fabrics is

   type Context_Access is access all Gela.Plain_Contexts.Context;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Parameters : League.String_Vectors.Universal_String_Vector)
      return Gela.Contexts.Context_Access
   is
      pragma Unreferenced (Parameters);
      Result : constant Context_Access := new Gela.Plain_Contexts.Context;
   begin
      return Gela.Contexts.Context_Access (Result);
   end Create_Context;

end Gela.Context_Fabrics;
