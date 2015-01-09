with Gela.Plain_Contexts;

package body Gela.Context_Factories is

   type Context_Access is access all Gela.Plain_Contexts.Context;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Parameters : League.String_Vectors.Universal_String_Vector;
      Include    : League.Strings.Universal_String)
      return Gela.Contexts.Context_Access
   is
      Result : constant Context_Access := new Gela.Plain_Contexts.Context;
      Param  : League.Strings.Universal_String;
      Path   : League.Strings.Universal_String;
      Name   : League.Strings.Universal_String;
      Debug  : League.Strings.Universal_String;
   begin
      for J in 1 .. Parameters.Length loop
         Param := Parameters.Element (J);
         if Param.Starts_With ("-I") then
            if not Path.Is_Empty then
               Path.Append (":");
            end if;

            Path.Append (Param.Slice (3, Param.Length));
         elsif Param.Starts_With ("--debug=") then
            Debug.Append (Param.Slice (9, Param.Length));
         else
            Name := Param;
         end if;
      end loop;

      Result.Initialize (Include, Path, Name, Debug);

      return Gela.Contexts.Context_Access (Result);
   end Create_Context;

end Gela.Context_Factories;
