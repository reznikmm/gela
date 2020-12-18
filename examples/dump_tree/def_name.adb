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
with Program.Lexical_Elements;
with Program.Plain_Contexts;

with Program.Storage_Pools.Instance;
pragma Unreferenced (Program.Storage_Pools.Instance);

procedure Def_Name is

   type Visitor is new Program.Element_Visitors.Element_Visitor with record
      Level : Natural := 0;
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

   procedure Process_Units
     (List : Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access);

   procedure Process_Unit
     (Unit : Program.Compilation_Units.Compilation_Unit_Access);

   procedure Traverse (Element : Program.Elements.Element_Access);

   ----------------
   -- Identifier --
   ----------------

   procedure Identifier
     (Self    : in out Visitor;
      Element : not null Program.Elements.Identifiers.Identifier_Access)
   is
      pragma Unreferenced (Self);
      Text : Program.Elements.Identifiers.Identifier_Text_Access;
      Token : Program.Lexical_Elements.Lexical_Element_Access;
      Def : Program.Elements.Defining_Identifiers.Defining_Identifier_Access;
      Def_Text : Program.Elements.Defining_Identifiers.
        Defining_Identifier_Text_Access;
   begin
      if not Element.Is_Part_Of_Implicit then
         Text := Element.To_Identifier_Text;
         Token := Text.Identifier_Token;
         Ada.Wide_Wide_Text_IO.Put (Token.Image);
         Ada.Wide_Wide_Text_IO.Put (" ");
         Ada.Wide_Wide_Text_IO.Put (Token.From_Image);
         Ada.Wide_Wide_Text_IO.Put (": ");

         Def := Element.Corresponding_Defining_Identifier;

         if not Def.Assigned then
            Ada.Wide_Wide_Text_IO.Put_Line ("-");
         elsif Def.Is_Part_Of_Implicit then
            Ada.Wide_Wide_Text_IO.Put_Line (" implicit");
         else
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

   procedure Process_Unit
     (Unit : Program.Compilation_Units.Compilation_Unit_Access) is
   begin
      for J in 1 .. Ada.Command_Line.Argument_Count loop
         if Unit.Compilation.Text_Name =
           Ada.Characters.Conversions.To_Wide_Wide_String
             (Ada.Command_Line.Argument (J))
         then
            Ada.Wide_Wide_Text_IO.Put_Line ("Unit: " & Unit.Full_Name);

            for Item in Unit.Context_Clause_Elements.Each_Element loop
               Traverse (Item.Element);
            end loop;

            Traverse (Unit.Unit_Declaration);
            exit;
         end if;
      end loop;
   end Process_Unit;

   procedure Process_Units
     (List : Program.Compilation_Unit_Vectors.Compilation_Unit_Vector_Access)
   is
   begin
      for Cursor in List.Each_Unit loop
         Process_Unit (Cursor.Unit);
      end loop;
   end Process_Units;

   procedure Traverse (Element : Program.Elements.Element_Access) is
      Printer : Visitor;
   begin
      for Cursor in Element.Each_Child loop
         Cursor.Element.Visit (Printer);
         Traverse (Cursor.Element);
      end loop;
   end Traverse;

   Ctx  : aliased Program.Plain_Contexts.Context;
begin
   Ctx.Initialize;

   for J in 1 .. Ada.Command_Line.Argument_Count loop
      Ctx.Parse_File
        (Ada.Characters.Conversions.To_Wide_Wide_String
           (Ada.Command_Line.Argument (J)));
   end loop;

   Ctx.Complete_Analysis;

   Process_Units (Ctx.Library_Unit_Declarations);
   Process_Units (Ctx.Compilation_Unit_Bodies);
end Def_Name;
