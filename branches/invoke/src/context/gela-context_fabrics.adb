with League.Strings;

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
      Result : constant Context_Access := new Gela.Plain_Contexts.Context;
      Param  : League.Strings.Universal_String;
      Path   : League.Strings.Universal_String;
   begin
      for J in 1 .. Parameters.Length loop
         Param := Parameters.Element (J);
         if Param.Starts_With ("-I") then
            if not Path.Is_Empty then
               Path.Append (":");
            end if;

            Path.Append (Param.Slice (3, Param.Length));
         end if;
      end loop;

      Result.Initialize (Path);

      return Gela.Contexts.Context_Access (Result);
   end Create_Context;

end Gela.Context_Fabrics;
