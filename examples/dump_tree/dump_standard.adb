--  SPDX-FileCopyrightText: 2019-2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Characters.Conversions;
with Ada.Command_Line;
with Ada.Wide_Wide_Text_IO;

with Program.Compilation_Unit_Vectors;
with Program.Compilation_Units;
with Program.Elements.Defining_Names;
with Errors;
with Program.Plain_Contexts;
with Program.Visibility;

with Program.Storage_Pools.Instance;
pragma Unreferenced (Program.Storage_Pools.Instance);

procedure Dump_Standard is

   procedure Print (View : Program.Visibility.View);
   procedure Print (View : Program.Visibility.View_Array);

   Err : aliased Errors.Error_Listener;
   Ctx : aliased Program.Plain_Contexts.Context;

   -----------
   -- Print --
   -----------

   procedure Print (View : Program.Visibility.View_Array) is
   begin
      for J in  View'Range loop
         Print (View (J));
      end loop;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (View : Program.Visibility.View) is
      use Program.Visibility;
      Name : constant Program.Elements.Defining_Names.Defining_Name_Access :=
        Program.Visibility.Name (View);
   begin
      if Name.Assigned then
         Ada.Wide_Wide_Text_IO.Put_Line
           (Name.Image
            & " ["
            & View_Kind'Wide_Wide_Image (View.Kind)
            & "]");
      else
         Ada.Wide_Wide_Text_IO.Put_Line
           ("___ ["
            & View_Kind'Wide_Wide_Image (View.Kind)
            & "]");
      end if;

      case View.Kind is
         when Enumeration_Type_View =>
            Print (Enumeration_Literals (View));
         when Signed_Integer_Type_View =>
            null;
         when Modular_Type_View =>
            null;
         when Float_Point_Type_View =>
            null;
         when Array_Type_View =>
            declare
               Indexes : constant Program.Visibility.View_Array :=
                 Program.Visibility.Indexes (View);
            begin
               for J in Indexes'Range loop
                  Ada.Wide_Wide_Text_IO.Put ("   ");
                  Print (Indexes (J));
               end loop;
               Ada.Wide_Wide_Text_IO.Put (" => ");
               Print (Program.Visibility.Component (View));
            end;
         when Implicit_Type_View =>
            null;
         when Enumeration_Literal_View =>
            null;
         when Character_Literal_View =>
            null;
         when Subtype_View =>
            Ada.Wide_Wide_Text_IO.Put ("   ");
            Print (Program.Visibility.Subtype_Mark (View));
         when Parameter_View =>
            Ada.Wide_Wide_Text_IO.Put ("   ");
            Print (Program.Visibility.Subtype_Mark (View));
            if Program.Visibility.Has_Default (View) then
               Ada.Wide_Wide_Text_IO.Put ("?");
            end if;
         when Exception_View =>
            null;
         when Package_View | Procedure_View =>
            Print (Region_Items (View));
      end case;
   end Print;

   Last : Positive := Ada.Command_Line.Argument_Count + 1;
begin
   Ctx.Initialize (Err'Unchecked_Access);

   for J in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         Arg : constant Wide_Wide_String :=
           Ada.Characters.Conversions.To_Wide_Wide_String
             (Ada.Command_Line.Argument (J));
      begin
         if Arg'Length > 2 and then Arg (1 .. 2) = "-I" then
            Ctx.Add_Search_Directory (Arg (3 .. Arg'Last));
         elsif Arg = "--" then
            Last := J + 1;
            exit;
         else
            Ctx.Parse_File (Arg);
         end if;
      end;
   end loop;

   Ctx.Complete_Analysis;

   if Last > Ada.Command_Line.Argument_Count then
      Print (Ctx.Immediate_Visible ("", "Standard"));
   else
      declare
         Unit : constant Wide_Wide_String :=
           Ada.Characters.Conversions.To_Wide_Wide_String
             (Ada.Command_Line.Argument (Last));
         Name : constant Wide_Wide_String :=
           Ada.Characters.Conversions.To_Wide_Wide_String
             (Ada.Command_Line.Argument (Last + 1));
      begin
         Print (Ctx.Immediate_Visible (Unit, Name));
      end;
   end if;
end Dump_Standard;
