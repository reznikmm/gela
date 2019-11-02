--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Characters.Conversions;
with Ada.Command_Line;
with Ada.Wide_Wide_Text_IO;

with Program.Compilation_Unit_Vectors;
with Program.Compilation_Units;
with Program.Plain_Contexts;
with Program.Visibility;

with Program.Storage_Pools.Instance;
pragma Unreferenced (Program.Storage_Pools.Instance);

with Dump_Elements;

procedure Dump_Tree is

   File : constant String := Ada.Command_Line.Argument (1);

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

   Ctx  : aliased Program.Plain_Contexts.Context;
   Env  : aliased Program.Visibility.Context;
begin
   Ctx.Initialize;
   Ctx.Parse_File
     (Ada.Characters.Conversions.To_Wide_Wide_String (File), Env);

--   Ada.Wide_Wide_Text_IO.Put_Line ("Compilation: " & C.Text_Name);
--     Ada.Wide_Wide_Text_IO.Put_Line
--       ("Total lines:" & Natural'Wide_Wide_Image (C.Line_Count));
--     Ada.Wide_Wide_Text_IO.Put_Line
--       ("Total lexical elements:"
--        & Natural'Wide_Wide_Image (C.Lexical_Element_Count));

   Process_Units (Ctx.Library_Unit_Declarations);
   Process_Units (Ctx.Compilation_Unit_Bodies);
end Dump_Tree;
