with Ada.Wide_Text_IO;

with Asis;
with Asis.Ada_Environments;
with Asis.Compilation_Units;
with Asis.Errors;
with Asis.Exceptions;
with Asis.Implementation;

with League.Application;
with League.Strings;
with League.String_Vectors;

procedure Main is
   Args : constant League.String_Vectors.Universal_String_Vector :=
     League.Application.Arguments;
   Params  : League.Strings.Universal_String;
   Context : Asis.Context;
begin
   Params := Args.Join (' ');

   Asis.Implementation.Initialize ("");

   Asis.Ada_Environments.Associate
     (The_Context => Context,
      Name        => Asis.Ada_Environments.Default_Name,
      Parameters  => Params.To_UTF_16_Wide_String);

   Asis.Ada_Environments.Open (Context);

   declare
      List : constant Asis.Compilation_Unit_List :=
        Asis.Compilation_Units.Compilation_Units (Context);
   begin
      for J in List'Range loop
         Ada.Wide_Text_IO.Put_Line
           ("Text_Name: " & Asis.Compilation_Units.Text_Name (List (J)));
         Ada.Wide_Text_IO.Put_Line
           ("Unit_Kind: " & Asis.Unit_Kinds'Wide_Image
              (Asis.Compilation_Units.Unit_Kind (List (J))));
         Ada.Wide_Text_IO.Put_Line
           ("Unit_Class: " & Asis.Unit_Classes'Wide_Image
              (Asis.Compilation_Units.Unit_Class (List (J))));
         Ada.Wide_Text_IO.Put_Line
           ("Unit_Origin: " & Asis.Unit_Origins'Wide_Image
              (Asis.Compilation_Units.Unit_Origin (List (J))));
         Ada.Wide_Text_IO.Put_Line
           ("Unit_Full_Name: " &
              Asis.Compilation_Units.Unit_Full_Name (List (J)));
         Ada.Wide_Text_IO.Put_Line
           ("Unique_Name: " &
              Asis.Compilation_Units.Unique_Name (List (J)));
         Ada.Wide_Text_IO.Put_Line
           ("Command_Line: " &
              Asis.Compilation_Units.Compilation_Command_Line_Options
              (List (J)));
         Ada.Wide_Text_IO.New_Line;
      end loop;
   end;

   Ada.Wide_Text_IO.Put_Line
     (Asis.Ada_Environments.Debug_Image (Context));

   Asis.Ada_Environments.Close        (Context);
   Asis.Ada_Environments.Dissociate   (Context);
   Asis.Implementation.Finalize       ("");

exception
   when Asis.Exceptions.ASIS_Failed =>
      Ada.Wide_Text_IO.Put_Line
        ("ASIS_Failed status: " &
           Asis.Errors.Error_Kinds'Wide_Image
           (Asis.Implementation.Status));
      Ada.Wide_Text_IO.Put_Line (Asis.Implementation.Diagnosis);

end Main;
