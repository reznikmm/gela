--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Identifiers;
with Program.Elements.Parameter_Associations;
with Program.Elements.String_Literals;
with Program.Elements.Infix_Operators;
with Program.Interpretations.Names;
with Program.Node_Symbols;
with Program.Safe_Element_Visitors;
with Program.Symbols;
with Program.Visibility;

package body Program.Complete_Contexts is

   function Up
     (Element : not null access Program.Elements.Element'Class;
      Sets    : not null Program.Interpretations.Context_Access)
        return Program.Interpretations.Interpretation_Set;

   package Up_Visitors is
      type Visitor
        (Sets : not null Program.Interpretations.Context_Access)
      is new
        Program.Safe_Element_Visitors.Safe_Element_Visitor
      with record
         Result : Program.Interpretations.Interpretation_Set;
      end record;

      overriding procedure Identifier
        (Self    : in out Visitor;
         Element : not null Program.Elements.Identifiers.Identifier_Access);

      overriding procedure Infix_Operator
        (Self    : in out Visitor;
         Element : not null Program.Elements.Infix_Operators
           .Infix_Operator_Access);

      overriding procedure Parameter_Association
        (Self    : in out Visitor;
         Element : not null Program.Elements.Parameter_Associations
                              .Parameter_Association_Access);

      overriding procedure String_Literal
        (Self    : in out Visitor;
         Element : not null Program.Elements.String_Literals
           .String_Literal_Access);

   end Up_Visitors;

   package body Up_Visitors is

      overriding procedure Identifier
        (Self    : in out Visitor;
         Element : not null Program.Elements.Identifiers.Identifier_Access)
      is
         Symbol : constant Program.Symbols.Symbol :=
           Program.Node_Symbols.Get_Symbol (Element);
      begin
         Self.Result := Self.Sets.Create_Interpretation_Set;
         Self.Result.Add_Symbol (Symbol);
      end Identifier;

      overriding procedure Infix_Operator
        (Self    : in out Visitor;
         Element : not null Program.Elements.Infix_Operators
         .Infix_Operator_Access)
      is
         use type Program.Interpretations.Names.Cursor;
         use all type Program.Visibility.View_Kind;
         Arity : constant Positive := 1 + Boolean'Pos (Element.Left.Assigned);
         Name  : Program.Interpretations.Interpretation_Set;
         Args  : Program.Interpretations.Interpretation_Set_Array (1 .. Arity);
         Down  : Program.Interpretations.Solution_Array (1 .. Arity + 1);
         Count : Natural := 0;
      begin
         Name := Up (Element.Operator, Self.Sets);
         if Element.Left.Assigned then
            Args (1) := Up (Element.Left, Self.Sets);
         end if;

         Args (Args'Last) := Up (Element.Right, Self.Sets);

         for N in Program.Interpretations.Names.Each (Name) loop
            declare
               View : constant Program.Visibility.View := +N;
            begin
               if View.Kind = Function_View then
                  declare
                     Params : constant Program.Visibility.View_Array :=
                       Program.Visibility.Parameters (View);
                  begin
                     if Params'Length = Args'Length then
                        Down (1) :=
                          (Program.Interpretations.Defining_Name_Solution,
                           View);

                        Self.Result.Add_Expression
                          (Program.Visibility.Result (View), Down);

                        Count := Count + 1;
                     end if;
                  end;
               end if;
            end;
         end loop;
      end Infix_Operator;

      overriding procedure Parameter_Association
        (Self    : in out Visitor;
         Element : not null Program.Elements.Parameter_Associations
                              .Parameter_Association_Access)
      is
      begin
         Self.Visit (Element.Actual_Parameter);
         pragma Assert (not Element.Formal_Parameter.Assigned);
      end Parameter_Association;

      overriding procedure String_Literal
        (Self    : in out Visitor;
         Element : not null Program.Elements.String_Literals
           .String_Literal_Access)
      is
         pragma Unreferenced (Element);
         use type Program.Visibility.View_Cursor;
      begin
         Self.Result := Self.Sets.Create_Interpretation_Set;

         for Std in Self.Sets.Env.Immediate_Visible
                      (Program.Symbols.Standard)
         loop
            for Str in Program.Visibility.Immediate_Visible
              (+Std, Program.Symbols.String)
            loop
               Self.Result.Add_Expression (+Str);
            end loop;
         end loop;
      end String_Literal;

   end Up_Visitors;

   package Down_Visitors is

      type Visitor
        (Setter : not null Program.Cross_Reference_Updaters
           .Cross_Reference_Updater_Access)
      is new
        Program.Safe_Element_Visitors.Safe_Element_Visitor
      with record
         Solution : Program.Interpretations.Solution;
      end record;

      overriding procedure Identifier
        (Self    : in out Visitor;
         Element : not null Program.Elements.Identifiers.Identifier_Access);

   end Down_Visitors;

   package body Down_Visitors is

      ----------------
      -- Identifier --
      ----------------

      overriding procedure Identifier
        (Self    : in out Visitor;
         Element : not null Program.Elements.Identifiers.Identifier_Access)
      is
      begin
         case Self.Solution.Kind is
            when Program.Interpretations.Defining_Name_Solution =>
               Self.Setter.Set_Corresponding_Defining_Name
                 (Element.all'Unchecked_Access,
                  Program.Visibility.Name
                    (Self.Solution.Name_View));
            when others =>
               raise Program_Error;
         end case;
      end Identifier;

   end Down_Visitors;

   --------------------
   -- Call_Statement --
   --------------------

   procedure Call_Statement
     (Sets    : not null Program.Interpretations.Context_Access;
      Setter  : not null Program.Cross_Reference_Updaters
                           .Cross_Reference_Updater_Access;
      Element : Program.Elements.Call_Statements.Call_Statement_Access)
   is
      use type Program.Interpretations.Names.Cursor;
      use all type Program.Visibility.View_Kind;
      --  pragma Warnings (Off);
      Name : Program.Interpretations.Interpretation_Set;
      Args : Program.Interpretations.Interpretation_Set_Array
        (1 .. Element.Parameters.Length);

      Count : Natural := 0;
      Name_Solution : Program.Interpretations.Solution;
   begin
      Name := Up (Element.Called_Name, Sets);

      for J in Element.Parameters.Each_Element loop
         Args (J.Index) := Up (J.Element, Sets);
      end loop;

      for N in Program.Interpretations.Names.Each (Name) loop
         declare
            View : constant Program.Visibility.View := +N;
         begin
            if View.Kind = Procedure_View then
               declare
                  Params : constant Program.Visibility.View_Array :=
                    Program.Visibility.Parameters (View);
               begin
                  if Params'Length = Args'Length then
                     Name_Solution :=
                       (Program.Interpretations.Defining_Name_Solution,
                        View);
                     Count := Count + 1;
                  end if;
               end;
            end if;
         end;
      end loop;

      if Count = 1 then
         declare
            Down : Down_Visitors.Visitor := (Setter, False, Name_Solution);
         begin
            Down.Visit (Element.Called_Name);
         end;
      end if;
   end Call_Statement;

   --------
   -- Up --
   --------

   function Up
     (Element : not null access Program.Elements.Element'Class;
      Sets    : not null Program.Interpretations.Context_Access)
        return Program.Interpretations.Interpretation_Set
   is
      Up : Up_Visitors.Visitor (Sets);
   begin
      Up.Visit (Element);
      return Up.Result;
   end Up;


end Program.Complete_Contexts;
