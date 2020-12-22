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
with Program.Node_Symbols;

package body Program.Unit_Dependencies is

   procedure Image
     (Lists  : in out Program.Symbol_Lists.Symbol_List_Table'Class;
      Name   : Program.Elements.Expressions.Expression_Access;
      Result : out Program.Symbol_Lists.Symbol_List);

   -----------------------------------
   -- Find_Elaboration_Dependencies --
   -----------------------------------

   procedure Find_Elaboration_Dependencies
     (Unit   : Program.Compilation_Units.Compilation_Unit_Access;
      Lists  : in out Program.Symbol_Lists.Symbol_List_Table'Class;
      Report : in out Unit_Dependency_Listener'Class) is
   begin
      raise Program_Error with "Unimplemented";
   end Find_Elaboration_Dependencies;

   -----------------------
   -- Find_Needed_Units --
   -----------------------

   procedure Find_Needed_Units
     (Unit   : Program.Compilation_Units.Compilation_Unit_Access;
      Lists  : in out Program.Symbol_Lists.Symbol_List_Table'Class;
      Report : in out Unit_Dependency_Listener'Class) is
   begin
      raise Program_Error with "Unimplemented";
   end Find_Needed_Units;

   --------------------------------
   -- Find_Semantic_Dependencies --
   --------------------------------

   procedure Find_Semantic_Dependencies
     (Unit   : Program.Compilation_Units.Compilation_Unit_Access;
      Lists  : in out Program.Symbol_Lists.Symbol_List_Table'Class;
      Report : in out Unit_Dependency_Listener'Class)
   is
      use type Program.Symbol_Lists.Symbol_List;

      Full_Name : Program.Symbol_Lists.Symbol_List;
      Clauses   : constant Program.Element_Vectors.Element_Vector_Access :=
        Unit.Context_Clause_Elements;
   begin
      Program.Node_Symbols.Unit_Full_Name (Lists, Unit, Full_Name);

      if Full_Name = Program.Symbol_Lists.Empty_Symbol_List then
         --  Standard package has no dependencies
         return;
      elsif Unit.Is_Subunit then
         --  A subunit depends semantically upon its parent body.
         Report.Required_Body (Lists.Prefix (Full_Name));
      else
         --  A library_item depends semantically upon its parent declaration.
         Report.Required_Declaration (Lists.Prefix (Full_Name));

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
            List : Program.Symbol_Lists.Symbol_List;
         begin
            for K in Names.Each_Element loop
               Image (Lists, Names.To_Expression (K.Index), List);
               Report.Required_Unit (List, Is_Limited);
            end loop;
         end;
      end loop;
   end Find_Semantic_Dependencies;

   procedure Image
     (Lists  : in out Program.Symbol_Lists.Symbol_List_Table'Class;
      Name   : Program.Elements.Expressions.Expression_Access;
      Result : out Program.Symbol_Lists.Symbol_List) is
   begin
      if Name.Is_Selected_Component then
         declare
            Prefix : Program.Symbol_Lists.Symbol_List;
         begin
            Image (Lists, Name.To_Selected_Component.Prefix, Prefix);
            Lists.Find_Or_Create
              (Prefix,
               Program.Node_Symbols.Get_Symbol
                 (Name.To_Selected_Component.Selector),
               Result);
         end;
      elsif Name.Is_Identifier then
         Lists.Find_Or_Create
           (Program.Symbol_Lists.Empty_Symbol_List,
            Program.Node_Symbols.Get_Symbol (Name),
            Result);
      else
         raise Program_Error;
      end if;
   end Image;

end Program.Unit_Dependencies;
