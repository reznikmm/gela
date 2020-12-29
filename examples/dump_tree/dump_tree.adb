--  SPDX-FileCopyrightText: 2019-2020 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Characters.Conversions;
with Ada.Command_Line;
with Ada.Wide_Wide_Text_IO;

with Program.Compilation_Unit_Vectors;
with Program.Compilation_Units;
with Program.Plain_Contexts;

with Program.Storage_Pools.Instance;
pragma Unreferenced (Program.Storage_Pools.Instance);

with Dump_Elements;
with Errors;

procedure Dump_Tree is

   procedure Process_Units
     (List : Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access);

   procedure Process_Unit
     (Unit : Program.Compilation_Units.Compilation_Unit_Access);

   procedure Process_Unit
     (Unit : Program.Compilation_Units.Compilation_Unit_Access) is
   begin
      Ada.Wide_Wide_Text_IO.Put_Line ("Unit: " & Unit.Full_Name);
      Dump_Elements.Print (Unit.Unit_Declaration);
   end Process_Unit;

   procedure Process_Units
     (List : Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access)
   is
   begin
      for Cursor in List.Each_Unit loop
         Process_Unit (Cursor.Unit);
      end loop;
   end Process_Units;

   Error : aliased Errors.Error_Listener;
   Ctx  : aliased Program.Plain_Contexts.Context;
begin
   Ctx.Initialize (Error'Unchecked_Access);

   for J in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         Arg : constant Wide_Wide_String :=
           Ada.Characters.Conversions.To_Wide_Wide_String
             (Ada.Command_Line.Argument (J));
      begin
         if Arg'Length > 2 and then Arg (1 .. 2) = "-I" then
            Ctx.Add_Search_Directory (Arg (3 .. Arg'Last));
         else
            Ctx.Parse_File (Arg);
         end if;
      end;
   end loop;

   Ctx.Complete_Analysis;

--   Ada.Wide_Wide_Text_IO.Put_Line ("Compilation: " & C.Text_Name);
--     Ada.Wide_Wide_Text_IO.Put_Line
--       ("Total lines:" & Natural'Wide_Wide_Image (C.Line_Count));
--     Ada.Wide_Wide_Text_IO.Put_Line
--       ("Total lexical elements:"
--        & Natural'Wide_Wide_Image (C.Lexical_Element_Count));

   Process_Units (Ctx.Library_Unit_Declarations);
   Process_Units (Ctx.Compilation_Unit_Bodies);
end Dump_Tree;
