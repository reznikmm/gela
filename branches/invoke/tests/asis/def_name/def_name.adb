with Ada.Wide_Text_IO;
with Ada.Command_Line;
with Ada.Strings.Wide_Fixed;

with Asis;
with Asis.Ada_Environments;
with Asis.Clauses;
with Asis.Compilation_Units;
with Asis.Elements;
with Asis.Errors;
with Asis.Exceptions;
with Asis.Expressions;
with Asis.Implementation;

with League.Application;
with League.Strings;
with League.String_Vectors;

procedure Def_Name is
   procedure On_Unit (Unit : Asis.Compilation_Unit);

   -------------
   -- On_Unit --
   -------------

   procedure On_Unit (Unit : Asis.Compilation_Unit) is
      Withs : constant Asis.Element_List :=
        Asis.Elements.Context_Clause_Elements (Unit);
   begin
      for J in Withs'Range loop
         case Asis.Elements.Clause_Kind (Withs (J)) is
            when Asis.A_With_Clause =>
               declare
                  Names : constant Asis.Element_List :=
                    Asis.Clauses.Clause_Names (Withs (J));
                  Def : constant Asis.Defining_Name :=
                    Asis.Expressions.Corresponding_Name_Definition (Names (1));
               begin
                  Ada.Wide_Text_IO.Put_Line
                    (Asis.Expressions.Name_Image (Names (1)));
                  Ada.Wide_Text_IO.Put_Line
                    (Asis.Element_Kinds'Wide_Image
                       (Asis.Elements.Element_Kind (Def)));
               end;
            when others =>
               null;
         end case;
      end loop;
   end On_Unit;

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
      Name : constant Wide_String := "/" &
        Args.Element (Args.Length).To_UTF_16_Wide_String;
      List : constant Asis.Compilation_Unit_List :=
        Asis.Compilation_Units.Compilation_Units (Context);
   begin
      for J in List'Range loop
         if Name = Ada.Strings.Wide_Fixed.Tail
           (Source => Asis.Compilation_Units.Text_Name (List (J)),
            Count  => Name'Length)
         then
            On_Unit (List (J));
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
end Def_Name;
