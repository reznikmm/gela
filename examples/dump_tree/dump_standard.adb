--  SPDX-FileCopyrightText: 2019-2021 Max Reznik <reznikmm@gmail.com>
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

   type Verbosity is
     (Short,   --  Just a name
      Normal,  --  One line
      Long);   --  Including nested items

   procedure Print
     (View    : Program.Visibility.View;
      Padding : Wide_Wide_String := "";
      Verbose : Verbosity);

   procedure Print
     (View    : Program.Visibility.View_Array;
      Padding : Wide_Wide_String;
      Verbose : Verbosity);

   procedure Print
     (View    : Program.Visibility.View_Iterator;
      Padding : Wide_Wide_String;
      Verbose : Verbosity);

   Indent : constant Wide_Wide_String := "  ";

   Err : aliased Errors.Error_Listener;
   Ctx : aliased Program.Plain_Contexts.Context;

   -----------
   -- Print --
   -----------

   procedure Print
     (View    : Program.Visibility.View_Array;
      Padding : Wide_Wide_String;
      Verbose : Verbosity) is
   begin
      for J in View'Range loop
         Ada.Wide_Wide_Text_IO.New_Line;
         Print (View (J), Padding, Verbose);
      end loop;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (View    : Program.Visibility.View_Iterator;
      Padding : Wide_Wide_String;
      Verbose : Verbosity)
   is
      use type Program.Visibility.View_Cursor;
   begin
      for J in View loop
         Ada.Wide_Wide_Text_IO.New_Line;
         Print (+J, Padding, Verbose);
      end loop;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (View    : Program.Visibility.View;
      Padding : Wide_Wide_String := "";
      Verbose : Verbosity)
   is
      use Program.Visibility;
      Name : constant Program.Elements.Defining_Names.Defining_Name_Access :=
        Program.Visibility.Name (View);
   begin
      if Verbose = Short then
         Ada.Wide_Wide_Text_IO.Put (" ");
      else
         Ada.Wide_Wide_Text_IO.Put (Padding);
      end if;

      if Name.Assigned then
         Ada.Wide_Wide_Text_IO.Put (Name.Image);
      else
         Ada.Wide_Wide_Text_IO.Put ("___");
      end if;

      if Verbose = Short and Name.Assigned then
         return;
      end if;

      Ada.Wide_Wide_Text_IO.Put
        (" ["
         & View_Kind'Wide_Wide_Image (View.Kind)
         & "]");

      case View.Kind is
         when Unresolved_View =>
            null;
         when Enumeration_Type_View =>
            if Verbose = Long then
               Print (Enumeration_Literals (View), Padding & Indent, Normal);
            end if;
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
                  Print (Indexes (J), Padding & Indent, Short);
               end loop;
               Ada.Wide_Wide_Text_IO.Put (" =>");
               Print
                 (Program.Visibility.Component (View),
                  Padding & Indent,
                  Short);
            end;
         when Implicit_Type_View =>
            null;
         when Enumeration_Literal_View =>
            null;
         when Character_Literal_View =>
            null;
         when Subtype_View =>
            Print
              (Program.Visibility.Subtype_Mark (View),
               Padding & Indent,
               Short);
         when Parameter_View | Component_View =>
            Print
              (Program.Visibility.Subtype_Mark (View),
               Padding & Indent,
               Verbosity'Pred (Verbose));

            if Program.Visibility.Has_Default (View) then
               Ada.Wide_Wide_Text_IO.Put ("?");
            end if;
         when Variable_View =>
            Print
              (Program.Visibility.Subtype_Mark (View),
               Padding & Indent,
               Verbosity'Pred (Verbose));
         when Exception_View =>
            null;
         when Procedure_View .. Package_View | Record_Type_View =>
            if Verbose = Long then
               Print (Region_Items (View), Padding & Indent, Normal);
            end if;
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
      Print (Ctx.Immediate_Visible ("", "Standard"), "", Long);
   else
      declare
         Unit : constant Wide_Wide_String :=
           Ada.Characters.Conversions.To_Wide_Wide_String
             (Ada.Command_Line.Argument (Last));
         Name : constant Wide_Wide_String :=
           Ada.Characters.Conversions.To_Wide_Wide_String
             (Ada.Command_Line.Argument (Last + 1));
      begin
         Print (Ctx.Immediate_Visible (Unit, Name), "", Long);
      end;
   end if;
end Dump_Standard;
