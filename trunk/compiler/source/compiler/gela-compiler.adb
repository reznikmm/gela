with Ada.Text_IO;
with Ada.Wide_Text_IO;
with Ada.Command_Line;

with Asis;
with Asis.Ada_Environments;
with Asis.Compilation_Units;
with Asis.Elements;
with Asis.Errors;
with Asis.Exceptions;
with Asis.Implementation;

with League.Application;
with League.Strings;
with League.String_Vectors;

with Gela.Engines;
with Gela.Properties.Text;
with Gela.Rule.Register_All;

procedure Gela.Compiler is
   use type League.Strings.Universal_String;

   procedure Compile_Unit (Unit : Asis.Compilation_Unit);

   Args    : League.String_Vectors.Universal_String_Vector;
   Params  : League.Strings.Universal_String;
   File    : League.Strings.Universal_String;
   Context : Asis.Context;
   Engine  : constant Gela.Engines.Engine_Access := new Gela.Engines.Engine;

   ------------------
   -- Compile_Unit --
   ------------------

   procedure Compile_Unit (Unit : Asis.Compilation_Unit) is
      Decl : constant Asis.Declaration :=
        Asis.Elements.Unit_Declaration (Unit);
      Text : constant Gela.Properties.Text.Text :=
        Engine.Get (Decl, Gela.Properties.Code);
   begin
      Ada.Text_IO.Put_Line (Engine.Text_Container.Value (Text));
   end Compile_Unit;

begin
   for J in 1 .. League.Application.Arguments.Length loop
      declare
         Item : constant League.Strings.Universal_String :=
           League.Application.Arguments.Element (J);
      begin
         if not Item.Starts_With ("-") then
            File := "/" & Item;
         end if;

         Args.Append (Item);
      end;
   end loop;

   Params := Args.Join (' ');

   Asis.Implementation.Initialize ("");

   Asis.Ada_Environments.Associate
     (The_Context => Context,
      Name        => Asis.Ada_Environments.Default_Name,
      Parameters  => Params.To_UTF_16_Wide_String);

   Asis.Ada_Environments.Open (Context);
   Gela.Rule.Register_All (Engine.all);

   declare
      List : constant Asis.Compilation_Unit_List :=
        Asis.Compilation_Units.Compilation_Units (Context);
   begin
      for J in List'Range loop
         if League.Strings.From_UTF_16_Wide_String
           (Asis.Compilation_Units.Text_Name (List (J))).Ends_With (File)
         then
            Compile_Unit (List (J));
         end if;
      end loop;
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
end Gela.Compiler;
