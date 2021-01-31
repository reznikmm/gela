--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Identifiers;
with Program.Elements.Parameter_Associations;
with Program.Elements.String_Literals;
with Program.Elements.Infix_Operators;
with Program.Elements.Operator_Symbols;
with Program.Interpretations.Expressions;
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
   --  Visit subtree rooted at the Element and construct set of possible
   --  interpretations for it.

   procedure Down
     (Element  : not null access Program.Elements.Element'Class;
      Solution : Program.Interpretations.Solution;
      Setter   : not null Program.Cross_Reference_Updaters
                   .Cross_Reference_Updater_Access);
   --  Assign solution to the Element and all its children.

   procedure Resolve_Parameters
     (Arguments   : Program.Interpretations.Interpretation_Set_Array;
      Parameters  : Program.Visibility.View_Array;
      Callback    : access procedure
        (Down : Program.Interpretations.Solution_Array);
      Down        : in out Program.Interpretations.Solution_Array;
      Index       : Positive := 1);
   --  For each parameter starting from Parameters (Index), find an
   --  expression interpretation in Arguments provided that its type is an
   --  expected type of the parameter. Then fill corresponding solutions in
   --  Down and call Callback (Down).

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

      overriding procedure Operator_Symbol
        (Self    : in out Visitor;
         Element : not null Program.Elements.Operator_Symbols
           .Operator_Symbol_Access);

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
         Down  : Program.Interpretations.Solution_Array (0 .. Arity);
      begin
         Self.Result := Self.Sets.Create_Interpretation_Set;
         Name := Up (Element.Operator, Self.Sets);

         if Element.Left.Assigned then
            Args (1) := Up (Element.Left, Self.Sets);
         end if;

         Args (Args'Last) := Up (Element.Right, Self.Sets);

         for N in Program.Interpretations.Names.Each (Name) loop
            declare
               View : constant Program.Visibility.View := +N;

               procedure Callback (Down : Interpretations.Solution_Array);

               procedure Callback (Down : Interpretations.Solution_Array) is
               begin
                  Self.Result.Add_Expression
                    (Program.Visibility.Result (View), Down);
               end Callback;

            begin
               if View.Kind = Function_View then
                  Down (0) :=
                    (Program.Interpretations.Defining_Name_Solution,
                     View);

                  Resolve_Parameters
                    (Arguments  => Args,
                     Parameters => Program.Visibility.Parameters (View),
                     Callback   => Callback'Access,
                     Down       => Down);
               end if;
            end;
         end loop;
      end Infix_Operator;

      overriding procedure Operator_Symbol
        (Self    : in out Visitor;
         Element : not null Program.Elements.Operator_Symbols
           .Operator_Symbol_Access)
      is
         Symbol : constant Program.Symbols.Symbol :=
           Program.Node_Symbols.Get_Symbol (Element);
      begin
         Self.Result := Self.Sets.Create_Interpretation_Set;
         Self.Result.Add_Symbol (Symbol);
      end Operator_Symbol;

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

      overriding procedure Parameter_Association
        (Self    : in out Visitor;
         Element : not null Program.Elements.Parameter_Associations
                              .Parameter_Association_Access);

      overriding procedure String_Literal
        (Self    : in out Visitor;
         Element : not null Program.Elements.String_Literals
           .String_Literal_Access);

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

      overriding procedure Parameter_Association
        (Self    : in out Visitor;
         Element : not null Program.Elements.Parameter_Associations
                              .Parameter_Association_Access)
      is
      begin
         Self.Visit (Element.Actual_Parameter);
      end Parameter_Association;

      overriding procedure String_Literal
        (Self    : in out Visitor;
         Element : not null Program.Elements.String_Literals
           .String_Literal_Access) is
      begin
         null;
      end String_Literal;

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
      procedure Callback (Down : Program.Interpretations.Solution_Array);
      Name : Program.Interpretations.Interpretation_Set;
      Args : Program.Interpretations.Interpretation_Set_Array
        (1 .. Element.Parameters.Length);
      Found : Program.Interpretations.Solution_Array (0 .. Args'Length);
      Count : Natural := 0;

      procedure Callback (Down : Program.Interpretations.Solution_Array) is
      begin
         Count := Count + 1;
         Found := Down;
      end Callback;

   begin
      Name := Up (Element.Called_Name, Sets);

      for J in Element.Parameters.Each_Element loop
         Args (J.Index) := Up (J.Element, Sets);
      end loop;

      for N in Program.Interpretations.Names.Each (Name) loop
         declare
            View   : constant Program.Visibility.View := +N;
            Params : constant Program.Visibility.View_Array :=
              Program.Visibility.Parameters (View);
            Down   : Program.Interpretations.Solution_Array
              (0 .. Params'Length);
         begin
            if View.Kind = Procedure_View then
               Down (0) :=
                 (Program.Interpretations.Defining_Name_Solution,
                  View);

               Resolve_Parameters
                 (Arguments  => Args,
                  Parameters => Params,
                  Callback   => Callback'Access,
                  Down       => Down);
            end if;
         end;
      end loop;

      if Count = 1 then
         Down (Element.Called_Name, Found (0), Setter);

         for J in Element.Parameters.Each_Element loop
            Down (J.Element, Found (J.Index), Setter);
         end loop;
      end if;
   end Call_Statement;

   ----------
   -- Down --
   ----------

   procedure Down
     (Element  : not null access Program.Elements.Element'Class;
      Solution : Program.Interpretations.Solution;
      Setter   : not null Program.Cross_Reference_Updaters
        .Cross_Reference_Updater_Access)
   is
      Down : Down_Visitors.Visitor := (Setter, False, Solution);
   begin
      Down.Visit (Element);
   end Down;

   ------------------------
   -- Resolve_Parameters --
   ------------------------

   procedure Resolve_Parameters
     (Arguments   : Program.Interpretations.Interpretation_Set_Array;
      Parameters  : Program.Visibility.View_Array;
      Callback    : access procedure
        (Down : Program.Interpretations.Solution_Array);
      Down        : in out Program.Interpretations.Solution_Array;
      Index       : Positive := 1)
   is
      use type Program.Interpretations.Expressions.Cursor;
   begin
      if Arguments'Length /= Parameters'Length then
         return;
      elsif Index > Down'Last then
         Callback (Down);
         return;
      end if;

      for X in Program.Interpretations.Expressions.Each_Of_Type
        (Arguments (Index),
         Program.Visibility.Type_Of (Parameters (Index)))
      loop
         Down (Index) := -X;

         Resolve_Parameters
           (Arguments, Parameters, Callback, Down, Index + 1);
      end loop;
   end Resolve_Parameters;

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
