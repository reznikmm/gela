with League.String_Vectors;
with League.Strings;

package body Gela.GNAT_Naming_Schemas is

   ---------------
   -- Body_Name --
   ---------------

   overriding function Body_Name
     (Self   : Naming_Schema;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Naming_Schemas.File_Base_Name
   is
      List   : League.String_Vectors.Universal_String_Vector;
      Result : League.Strings.Universal_String;
   begin
      List := Self.Context.Symbols.Folded (Symbol).Split ('.');
      Result := List.Join ('-');
      Result.Append (".adb");

      return Result;
   end Body_Name;

   ----------------------
   -- Declaration_Name --
   ----------------------

   overriding function Declaration_Name
     (Self   : Naming_Schema;
      Symbol : Gela.Lexical_Types.Symbol)
      return Gela.Naming_Schemas.File_Base_Name
   is
      List   : League.String_Vectors.Universal_String_Vector;
      Result : League.Strings.Universal_String;
   begin
      List := Self.Context.Symbols.Folded (Symbol).Split ('.');
      Result := List.Join ('-');
      Result.Append (".ads");

      return Result;
   end Declaration_Name;

end Gela.GNAT_Naming_Schemas;
