--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Element_Filters;
with Program.Element_Vectors;
with Program.Elements.Identifiers;
with Program.Elements.Selected_Components;
with Program.Elements.With_Clauses;
with Program.Elements.Expressions;

package body Program.Unit_Dependencies is

   function Parent_Name (Name : Program.Text) return Program.Text;
   function Image
     (Name : Program.Elements.Expressions.Expression_Access)
      return Program.Text;

   -----------
   -- Image --
   -----------

   function Image
     (Name : Program.Elements.Expressions.Expression_Access)
      return Program.Text is
   begin
      if Name.Is_Selected_Component then
         declare
            Compount_Name : constant Program.Elements.Selected_Components
              .Selected_Component_Access := Name.To_Selected_Component;
         begin
            return Image (Compount_Name.Prefix) & "." &
              Image (Compount_Name.Selector);
         end;
      elsif Name.Is_Identifier then
         return Name.To_Identifier.Image;
      else
         raise Program_Error;
      end if;
   end Image;

   -----------------------------------
   -- Find_Elaboration_Dependencies --
   -----------------------------------

   procedure Find_Elaboration_Dependencies
     (Unit   : Program.Compilation_Units.Compilation_Unit_Access;
      Report : in out Unit_Dependency_Listener'Class) is
   begin
      raise Program_Error with "Unimplemented";
   end Find_Elaboration_Dependencies;

   -----------------------
   -- Find_Needed_Units --
   -----------------------

   procedure Find_Needed_Units
     (Unit   : Program.Compilation_Units.Compilation_Unit_Access;
      Report : in out Unit_Dependency_Listener'Class) is
   begin
      raise Program_Error with "Unimplemented";
   end Find_Needed_Units;

   --------------------------------
   -- Find_Semantic_Dependencies --
   --------------------------------

   procedure Find_Semantic_Dependencies
     (Unit   : Program.Compilation_Units.Compilation_Unit_Access;
      Report : in out Unit_Dependency_Listener'Class)
   is
      Full_Name : constant Program.Text := Unit.Full_Name;
      Clauses   : constant Program.Element_Vectors.Element_Vector_Access :=
        Unit.Context_Clause_Elements;
   begin
      if Full_Name = "" then
         --  Standard package has no dependencies
         return;
      elsif Unit.Is_Subunit then
         --  A subunit depends semantically upon its parent body.
         Report.Required_Body (Parent_Name (Full_Name));
      else
         --  A library_item depends semantically upon its parent declaration.
         Report.Required_Declaration (Parent_Name (Full_Name));

         if Unit.Is_Library_Unit_Body then
            --  A library_unit_body depends semantically upon the corresponding
            --  library_unit_declaration, if any.
            Report.Required_Declaration (Full_Name, If_Any => True);
         end if;
      end if;

      --  A compilation unit depends semantically upon each library_item
      --  mentioned in a with_clause of the compilation unit.
      for J in Clauses.Each_Element
        (Program.Element_Filters.Is_With_Clause'Access)
      loop
         declare
            With_Clause : constant Elements.With_Clauses.With_Clause_Access :=
              J.Element.To_With_Clause;
            Names : constant Program.Elements.Expressions
              .Expression_Vector_Access := With_Clause.Clause_Names;
            Is_Limited : constant Boolean := With_Clause.Has_Limited;
         begin
            for K in Names.Each_Element loop
               Report.Required_Unit
                 (Image (Names.To_Expression (K.Index)), Is_Limited);
            end loop;
         end;
      end loop;
   end Find_Semantic_Dependencies;

   -----------------
   -- Parent_Name --
   -----------------

   function Parent_Name (Name : Program.Text) return Program.Text is
   begin
      for J in reverse Name'Range loop
         if Name (J) = '.' then
            return Name (Name'First .. J - 1);
         end if;
      end loop;

      return "";
   end Parent_Name;

end Program.Unit_Dependencies;
