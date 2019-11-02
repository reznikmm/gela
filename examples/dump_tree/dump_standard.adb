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
with Program.Elements.Defining_Names;
with Program.Plain_Contexts;
with Program.Symbols;
with Program.Visibility;

with Program.Storage_Pools.Instance;
pragma Unreferenced (Program.Storage_Pools.Instance);

procedure Dump_Standard is

   procedure Print (View : Program.Visibility.View);
   procedure Print (View : Program.Visibility.View_Array);

   Ctx : aliased Program.Plain_Contexts.Context;
   Env : aliased Program.Visibility.Context;

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
         when Exception_View =>
            null;
         when Package_View =>
            Print (Immediate_Visible (View, Program.Symbols.Boolean));
            Print (Immediate_Visible (View, Program.Symbols.Integer));
            Print (Immediate_Visible (View, Ctx.Find ("Natural")));
            Print (Immediate_Visible (View, Ctx.Find ("Positive")));
            Print (Immediate_Visible (View, Program.Symbols.Float));
            Print (Immediate_Visible (View, Program.Symbols.Character));
            Print (Immediate_Visible (View, Program.Symbols.String));
            Print (Immediate_Visible (View, Ctx.Find ("Constraint_Error")));
      end case;
   end Print;

   File : constant String := Ada.Command_Line.Argument (1);

begin
   Ctx.Initialize;
   Ctx.Parse_File
     (Ada.Characters.Conversions.To_Wide_Wide_String (File),
      Env);

   Print (Env.Immediate_Visible (Program.Visibility.Standard));
end Dump_Standard;
