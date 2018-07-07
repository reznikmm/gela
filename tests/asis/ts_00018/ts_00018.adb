with Ada.Wide_Text_IO;
with Ada.Command_Line;

with Asis;
with Asis.Ada_Environments;
with Asis.Compilation_Units;
with Asis.Errors;
with Asis.Exceptions;
with Asis.Implementation;

with League.Application;
with League.Strings;
with League.String_Vectors;

procedure TS_00018 is
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
      pragma Unreferenced (List);
   begin
      null;
   end;

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
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end TS_00018;
