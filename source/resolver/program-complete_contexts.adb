--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Discrete_Simple_Expression_Ranges;
with Program.Elements.Identifiers;
with Program.Elements.Infix_Operators;
with Program.Elements.Numeric_Literals;
with Program.Elements.Operator_Symbols;
with Program.Elements.Parameter_Associations;
with Program.Elements.String_Literals;
with Program.Interpretations.Expressions;
with Program.Interpretations.Names;
with Program.Node_Symbols;
with Program.Safe_Element_Visitors;
with Program.Symbols;

package body Program.Complete_Contexts is

   package Up_Visitors is

      type Visitor
        (Sets : not null Program.Interpretations.Context_Access)
      is new
        Program.Safe_Element_Visitors.Safe_Element_Visitor
      with record
         Result : Program.Interpretations.Interpretation_Set;
      end record;

      overriding procedure Discrete_Simple_Expression_Range
        (Self    : in out Visitor;
         Element : not null Program.Elements.Discrete_Simple_Expression_Ranges
           .Discrete_Simple_Expression_Range_Access);

      overriding procedure Identifier
        (Self    : in out Visitor;
         Element : not null Program.Elements.Identifiers.Identifier_Access);

      overriding procedure Infix_Operator
        (Self    : in out Visitor;
         Element : not null Program.Elements.Infix_Operators
           .Infix_Operator_Access);

      overriding procedure Numeric_Literal
        (Self    : in out Visitor;
         Element : not null Program.Elements.Numeric_Literals
           .Numeric_Literal_Access);

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

      overriding procedure Discrete_Simple_Expression_Range
        (Self    : in out Visitor;
         Element : not null Program.Elements.Discrete_Simple_Expression_Ranges
           .Discrete_Simple_Expression_Range_Access)
      is
         use type Program.Interpretations.Expressions.Cursor;

         Left  : constant Program.Interpretations.Interpretation_Set :=
           Up (Element.Lower_Bound, Self.Sets);
         Right : constant Program.Interpretations.Interpretation_Set :=
           Up (Element.Upper_Bound, Self.Sets);
      begin
         Self.Result := Self.Sets.Create_Interpretation_Set;

         for N in Program.Interpretations.Expressions.Each (Left) loop
            for K in Program.Interpretations.Expressions.Each_Of_Type
              (Right, +N)
            loop
               Self.Result.Add_Expression
                 (+N,  --  FIXME: choose between +N, +K to avoid universal_int
                  (1 => -N, 2 => -K));
            end loop;
         end loop;
      end Discrete_Simple_Expression_Range;

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

      overriding procedure Numeric_Literal
        (Self    : in out Visitor;
         Element : not null Program.Elements.Numeric_Literals
           .Numeric_Literal_Access)
      is
         pragma Unreferenced (Element);
         use type Program.Visibility.View_Cursor;
      begin
         Self.Result := Self.Sets.Create_Interpretation_Set;

         for Std in Self.Sets.Env.Immediate_Visible
                      (Program.Symbols.Standard)
         loop
            for Str in Program.Visibility.Immediate_Visible
              (+Std, Program.Symbols.Integer)
            loop
               Self.Result.Add_Expression (+Str);
            end loop;
         end loop;
      end Numeric_Literal;

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

      overriding procedure Discrete_Simple_Expression_Range
        (Self    : in out Visitor;
         Element : not null Program.Elements.Discrete_Simple_Expression_Ranges
           .Discrete_Simple_Expression_Range_Access);

      overriding procedure Identifier
        (Self    : in out Visitor;
         Element : not null Program.Elements.Identifiers.Identifier_Access);

      overriding procedure Infix_Operator
        (Self    : in out Visitor;
         Element : not null Program.Elements.Infix_Operators
                              .Infix_Operator_Access);

      overriding procedure Numeric_Literal
        (Self    : in out Visitor;
         Element : not null Program.Elements.Numeric_Literals
           .Numeric_Literal_Access);

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

   end Down_Visitors;

   package body Down_Visitors is

      overriding procedure Discrete_Simple_Expression_Range
        (Self    : in out Visitor;
         Element : not null Program.Elements.Discrete_Simple_Expression_Ranges
           .Discrete_Simple_Expression_Range_Access) is
      begin
         Down (Element.Lower_Bound, Self.Solution.Tuple (1), Self.Setter);
         Down (Element.Upper_Bound, Self.Solution.Tuple (2), Self.Setter);
      end Discrete_Simple_Expression_Range;

      ----------------
      -- Identifier --
      ----------------

      overriding procedure Identifier
        (Self    : in out Visitor;
         Element : not null Program.Elements.Identifiers.Identifier_Access) is
      begin
         case Self.Solution.Kind is
            when Program.Interpretations.Defining_Name_Solution =>
               Self.Setter.Set_Corresponding_Defining_Name
                 (Element.all'Unchecked_Access,
                  Program.Visibility.Name (Self.Solution.Name_View));
            when others =>
               raise Program_Error;
         end case;
      end Identifier;

      overriding procedure Infix_Operator
        (Self    : in out Visitor;
         Element : not null Program.Elements.Infix_Operators
                              .Infix_Operator_Access) is
      begin
         Down (Element.Operator, Self.Solution.Tuple (0), Self.Setter);
         Down (Element.Left,     Self.Solution.Tuple (1), Self.Setter);
         Down (Element.Right,    Self.Solution.Tuple (2), Self.Setter);
      end Infix_Operator;

      overriding procedure Numeric_Literal
        (Self    : in out Visitor;
         Element : not null Program.Elements.Numeric_Literals
           .Numeric_Literal_Access) is
      begin
         null;
      end Numeric_Literal;

      overriding procedure Operator_Symbol
        (Self    : in out Visitor;
         Element : not null Program.Elements.Operator_Symbols
                              .Operator_Symbol_Access) is
      begin
         case Self.Solution.Kind is
            when Program.Interpretations.Defining_Name_Solution =>
               Self.Setter.Set_Corresponding_Defining_Name
                 (Element.all'Unchecked_Access,
                  Program.Visibility.Name (Self.Solution.Name_View));
            when others =>
               raise Program_Error;
         end case;
      end Operator_Symbol;

      overriding procedure Parameter_Association
        (Self    : in out Visitor;
         Element : not null Program.Elements.Parameter_Associations
                              .Parameter_Association_Access) is
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

   -------------------------
   -- Resolve_To_Any_Type --
   -------------------------

   procedure Resolve_To_Any_Type
     (Element : not null Program.Elements.Expressions.Expression_Access;
      Sets    : not null Program.Interpretations.Context_Access;
      Setter  : not null Program.Cross_Reference_Updaters
                           .Cross_Reference_Updater_Access;
      Result  : out Program.Visibility.View)
   is
      use type Program.Interpretations.Expressions.Cursor;

      Set : constant Program.Interpretations.Interpretation_Set :=
        Up (Element.all'Unchecked_Access, Sets);

      Found : Program.Interpretations.Solution;
      Count : Natural := 0;

   begin
      for N in Program.Interpretations.Expressions.Each (Set) loop
         Count := Count + 1;
         Found := -N;
         Result := +N;
      end loop;

      if Count > 0 then
         Down (Element.all'Unchecked_Access, Found, Setter);
      end if;
   end Resolve_To_Any_Type;

   ------------------------------
   -- Resolve_To_Expected_Type --
   ------------------------------

   procedure Resolve_To_Expected_Type
     (Element : not null Program.Elements.Element_Access;
      Sets    : not null Program.Interpretations.Context_Access;
      Setter  : not null Program.Cross_Reference_Updaters
                           .Cross_Reference_Updater_Access;
      Expect  : Program.Visibility.View)
   is
      use type Program.Interpretations.Expressions.Cursor;

      Set : constant Program.Interpretations.Interpretation_Set :=
        Up (Element, Sets);

      Found : Program.Interpretations.Solution;
      Count : Natural := 0;

   begin
      for N in Program.Interpretations.Expressions.Each_Of_Type
        (Set, Expect)
      loop
         Count := Count + 1;
         Found := -N;
      end loop;

      if Count > 0 then
         Down (Element.all'Unchecked_Access, Found, Setter);
      end if;
   end Resolve_To_Expected_Type;

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
