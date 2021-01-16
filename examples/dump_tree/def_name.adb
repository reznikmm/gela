--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Ada.Characters.Conversions;
with Ada.Command_Line;
with Ada.Wide_Wide_Text_IO;

with Program.Compilation_Unit_Vectors;
with Program.Compilation_Units;
with Program.Element_Iterators;
with Program.Element_Visitors;
with Program.Elements.Character_Literals;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Identifiers;
with Program.Elements.Operator_Symbols;
with Program.Elements.Procedure_Body_Declarations;
with Program.Lexical_Elements;
with Program.Plain_Contexts;

with Program.Storage_Pools.Instance;
pragma Unreferenced (Program.Storage_Pools.Instance);

with Errors;

procedure Def_Name is

   package Dump_Names is

      type Visitor is new Program.Element_Visitors.Element_Visitor with record
         Level    : Natural := 0;
         End_Name : Program.Elements.Element_Access;
      end record;

      procedure Identifier
        (Self    : in out Visitor;
         Element : not null Program.Elements.Identifiers.Identifier_Access);

      procedure Operator_Symbol
        (Self    : in out Visitor;
         Element : not null Program.Elements.Operator_Symbols
         .Operator_Symbol_Access);

      procedure Character_Literal
        (Self    : in out Visitor;
         Element : not null Program.Elements.Character_Literals
         .Character_Literal_Access);

      overriding procedure Procedure_Body_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Procedure_Body_Declarations
           .Procedure_Body_Declaration_Access);

   end Dump_Names;

   procedure Process_Units
     (List : Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access;
      Spec : Boolean);

   procedure Process_Unit
     (Unit : Program.Compilation_Units.Compilation_Unit_Access;
      Spec : Boolean);

   procedure Traverse
     (Element  : Program.Elements.Element_Access;
      End_Name : Program.Elements.Element_Access);

   procedure Enclosing_Unit_Name
     (Element : access Program.Elements.Element'Class);

   package body Dump_Names is

      ----------------
      -- Identifier --
      ----------------

      procedure Identifier
        (Self    : in out Visitor;
         Element : not null Program.Elements.Identifiers.Identifier_Access)
      is
         use type Program.Elements.Element_Access;
         Text     : Program.Elements.Identifiers.Identifier_Text_Access;
         Token    : Program.Lexical_Elements.Lexical_Element_Access;
         Def      : Program.Elements.Defining_Identifiers
           .Defining_Identifier_Access;
         Def_Text : Program.Elements.Defining_Identifiers.
           Defining_Identifier_Text_Access;
      begin
         if Self.End_Name = Program.Elements.Element_Access (Element) then
            null;
         elsif not Element.Is_Part_Of_Implicit then
            Text := Element.To_Identifier_Text;
            Token := Text.Identifier_Token;
            Ada.Wide_Wide_Text_IO.Put (Token.Image);
            Ada.Wide_Wide_Text_IO.Put (" ");
            Ada.Wide_Wide_Text_IO.Put (Token.From_Image);
            Ada.Wide_Wide_Text_IO.Put (" => ");

            Def := Element.Corresponding_Defining_Identifier;

            if not Def.Assigned then
               Ada.Wide_Wide_Text_IO.Put_Line ("-");
            elsif Def.Is_Part_Of_Implicit then
               Ada.Wide_Wide_Text_IO.Put_Line (" implicit");
            else
               Enclosing_Unit_Name (Def);
               Ada.Wide_Wide_Text_IO.Put (" ");
               Def_Text := Def.To_Defining_Identifier_Text;
               Token := Def_Text.Identifier_Token;
               Ada.Wide_Wide_Text_IO.Put_Line (Token.From_Image);
            end if;
         end if;
      end Identifier;

      ---------------------
      -- Operator_Symbol --
      ---------------------

      procedure Operator_Symbol
        (Self    : in out Visitor;
         Element : not null Program.Elements.Operator_Symbols
         .Operator_Symbol_Access) is
      begin
         null;
      end Operator_Symbol;

      -----------------------
      -- Character_Literal --
      -----------------------

      procedure Character_Literal
        (Self    : in out Visitor;
         Element : not null Program.Elements.Character_Literals
         .Character_Literal_Access) is
      begin
         null;
      end Character_Literal;

      overriding procedure Procedure_Body_Declaration
        (Self    : in out Visitor;
         Element : not null Program.Elements.Procedure_Body_Declarations
           .Procedure_Body_Declaration_Access)
      is
      begin
         Self.End_Name := Program.Elements.Element_Access (Element.End_Name);
      end Procedure_Body_Declaration;

   end Dump_Names;

   procedure Process_Unit
     (Unit : Program.Compilation_Units.Compilation_Unit_Access;
      Spec : Boolean) is
   begin
      for J in 1 .. Ada.Command_Line.Argument_Count loop
         if Unit.Compilation.Text_Name =
           Ada.Characters.Conversions.To_Wide_Wide_String
             (Ada.Command_Line.Argument (J))
         then
            if Spec then
               Ada.Wide_Wide_Text_IO.Put ("spec");
            else
               Ada.Wide_Wide_Text_IO.Put ("body");
            end if;

            Ada.Wide_Wide_Text_IO.Put (" ");
            Ada.Wide_Wide_Text_IO.Put_Line (Unit.Full_Name);

            for Item in Unit.Context_Clause_Elements.Each_Element loop
               Traverse (Item.Element, null);
            end loop;

            Traverse (Unit.Unit_Declaration, null);
            exit;
         end if;
      end loop;
   end Process_Unit;

   procedure Process_Units
     (List : Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access;
      Spec : Boolean) is
   begin
      for Cursor in List.Each_Unit loop
         Process_Unit (Cursor.Unit, Spec);
      end loop;
   end Process_Units;

   procedure Traverse
     (Element  : Program.Elements.Element_Access;
      End_Name : Program.Elements.Element_Access)
   is
      Printer : Dump_Names.Visitor;
   begin
      Printer.End_Name := End_Name;
      Element.Visit (Printer);

      for Cursor in Element.Each_Child loop
         Traverse (Cursor.Element, Printer.End_Name);
      end loop;
   end Traverse;

   Ctx : aliased Program.Plain_Contexts.Context;

   procedure Enclosing_Unit_Name
     (Element : access Program.Elements.Element'Class)
   is
      use type Program.Elements.Element_Access;
      Parent : Program.Elements.Element_Access := Element;
   begin
      while Parent.Enclosing_Element.Assigned loop
         Parent := Parent.Enclosing_Element;
      end loop;

      for Cursor in Ctx.Library_Unit_Declarations.Each_Unit loop
         if Cursor.Unit.Unit_Declaration = Parent then
            Ada.Wide_Wide_Text_IO.Put (Cursor.Unit.Full_Name);
            return;
         end if;
      end loop;

      for Cursor in Ctx.Compilation_Unit_Bodies.Each_Unit loop
         if Cursor.Unit.Unit_Declaration = Parent then
            Ada.Wide_Wide_Text_IO.Put (Cursor.Unit.Full_Name);
            return;
         end if;
      end loop;
   end Enclosing_Unit_Name;

   Err : aliased Errors.Error_Listener;
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
         else
            Ctx.Parse_File (Arg);
         end if;
      end;
   end loop;

   Ctx.Complete_Analysis;

   Process_Units (Ctx.Library_Unit_Declarations, True);
   Process_Units (Ctx.Compilation_Unit_Bodies, False);
end Def_Name;
