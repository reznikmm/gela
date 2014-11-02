------------------------------------------------------------------------------
--                           G E L A   A S I S                              --
--       ASIS implementation for Gela project, a portable Ada compiler      --
--                        a portable Ada compiler                           --
--                        http://gela.ada-ru.org/                           --
--                     - - - - - - - - - - - - - - -                        --
--              Read copyright and license in gela.ads file                 --
------------------------------------------------------------------------------

with League.String_Vectors;

package body Gela.Name_Schemas.GNAT is

   ---------------
   -- Body_Name --
   ---------------

   overriding function Body_Name
     (Self  : access GNAT_Name_Schema;
      Unit  : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
      List : League.String_Vectors.Universal_String_Vector;
      Result : League.Strings.Universal_String;
   begin
      List := Unit.To_Lowercase.Split ('.');
      Result := List.Join ('-');
      Result.Append (".adb");

      return Result;
   end Body_Name;

   ----------------------
   -- Declaration_Name --
   ----------------------

   overriding function Declaration_Name
     (Self  : access GNAT_Name_Schema;
      Unit  : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      pragma Unreferenced (Self);
      List : League.String_Vectors.Universal_String_Vector;
      Result : League.Strings.Universal_String;
   begin
      List := Unit.To_Lowercase.Split ('.');
      Result := List.Join ('-');
      Result.Append (".ads");

      return Result;
   end Declaration_Name;

end Gela.Name_Schemas.GNAT;
