--  SPDX-FileCopyrightText: 2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Elements.Discrete_Simple_Expression_Ranges;
with Program.Elements.Function_Calls;
with Program.Elements.Identifiers;
with Program.Elements.Infix_Operators;
with Program.Elements.Numeric_Literals;
with Program.Elements.Operator_Symbols;
with Program.Elements.Parameter_Associations;
with Program.Elements.Record_Component_Associations;
with Program.Elements.Record_Aggregates;
with Program.Elements.String_Literals;
with Program.Interpretations.Expressions;
with Program.Interpretations.Names;
with Program.Node_Symbols;
with Program.Resolvers.Name_In_Region;
with Program.Safe_Element_Visitors;
with Program.Symbols;
with Program.Type_Matchers;

package body Program.Complete_Contexts is

   package Up_Visitors is

      type Visitor
        (Sets : not null Program.Interpretations.Context_Access)
      is new
        Program.Safe_Element_Visitors.Safe_Element_Visitor
      with record
         Result : Program.Interpretations.Interpretation_Set;
      end record;

      procedure Call_Up
        (Self   : in out Visitor'Class;
         Prefix : not null Program.Elements.Element_Access;
         Arg    : Program.Element_Vectors.Iterators.Forward_Iterator'Class;
         Arity  : Positive);
      --  Common procedure from Infix_Operator and Function_Call

      overriding procedure Discrete_Simple_Expression_Range
        (Self    : in out Visitor;
         Element : not null Program.Elements.Discrete_Simple_Expression_Ranges
           .Discrete_Simple_Expression_Range_Access);

      overriding procedure Function_Call
        (Self    : in out Visitor;
         Element : not null Program.Elements.Function_Calls
           .Function_Call_Access);

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

      overriding procedure Record_Aggregate
        (Self    : in out Visitor;
         Element : not null Program.Elements.Record_Aggregates
                              .Record_Aggregate_Access);

      overriding procedure String_Literal
        (Self    : in out Visitor;
         Element : not null Program.Elements.String_Literals
           .String_Literal_Access);

   end Up_Visitors;

   package body Up_Visitors is

      procedure Call_Up
        (Self   : in out Visitor'Class;
         Prefix : not null Program.Elements.Element_Access;
         Arg    : Program.Element_Vectors.Iterators.Forward_Iterator'Class;
         Arity  : Positive)
      is
         use type Program.Interpretations.Names.Cursor;
         use all type Program.Visibility.View_Kind;
         Name  : Program.Interpretations.Interpretation_Set;
         Args  : Program.Interpretations.Interpretation_Set_Array (1 .. Arity);
         Down  : Program.Interpretations.Solution_Array (0 .. Arity);
      begin
         Self.Result := Self.Sets.Create_Interpretation_Set;
         Name := Up (Prefix, Self.Sets);

         for J in Arg loop
            Args (J.Index) := Up (J.Element, Self.Sets);
         end loop;

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
      end Call_Up;

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

      overriding procedure Function_Call
        (Self    : in out Visitor;
         Element : not null Program.Elements.Function_Calls
           .Function_Call_Access) is
      begin
         Self.Call_Up
           (Prefix => Element.Prefix.To_Element,
            Arg    => Element.Parameters.Each_Element,
            Arity  => Element.Parameters.Length);
         null;
      end Function_Call;

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
         Arity : constant Positive := 1 + Boolean'Pos (Element.Left.Assigned);
      begin
         if Arity = 1 then
            Self.Call_Up
              (Prefix => Element.Operator.To_Element,
               Arg    => Program.Element_Vectors.Single_Element
                 (Element.Right.To_Element),
               Arity  => Arity);
         else
            Self.Call_Up
              (Prefix => Element.Operator.To_Element,
               Arg    => Program.Element_Vectors.Two_Elements
                 (Element.Left.To_Element,
                  Element.Right.To_Element),
               Arity  => Arity);
         end if;
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

      Only_Records : aliased Program.Type_Matchers.Record_Type_Matcher;

      overriding procedure Record_Aggregate
        (Self    : in out Visitor;
         Element : not null Program.Elements.Record_Aggregates
                              .Record_Aggregate_Access)
      is
         pragma Unreferenced (Element);
      begin
         Self.Result := Self.Sets.Create_Interpretation_Set;
         Self.Result.Add_Expression_Category (Only_Records'Access);
      end Record_Aggregate;

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
        (Sets   : not null Program.Interpretations.Context_Access;
         Setter : not null Program.Cross_Reference_Updaters
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

      overriding procedure Function_Call
        (Self    : in out Visitor;
         Element : not null Program.Elements.Function_Calls
           .Function_Call_Access);

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

      overriding procedure Record_Aggregate
        (Self    : in out Visitor;
         Element : not null Program.Elements.Record_Aggregates
                              .Record_Aggregate_Access);

      overriding procedure String_Literal
        (Self    : in out Visitor;
         Element : not null Program.Elements.String_Literals
           .String_Literal_Access);

   end Down_Visitors;

   package body Down_Visitors is

      procedure Down
        (Self     : in out Visitor'Class;
         Element  : not null access Program.Elements.Element'Class;
         Solution : Program.Interpretations.Solution);

      procedure Down
        (Self     : in out Visitor'Class;
         Element  : not null access Program.Elements.Element'Class;
         Solution : Program.Interpretations.Solution) is
      begin
         Down (Element, Solution, Self.Setter, Self.Sets);
      end Down;

      overriding procedure Discrete_Simple_Expression_Range
        (Self    : in out Visitor;
         Element : not null Program.Elements.Discrete_Simple_Expression_Ranges
           .Discrete_Simple_Expression_Range_Access) is
      begin
         Self.Down (Element.Lower_Bound, Self.Solution.Tuple (1));
         Self.Down (Element.Upper_Bound, Self.Solution.Tuple (2));
      end Discrete_Simple_Expression_Range;

      overriding procedure Function_Call
        (Self    : in out Visitor;
         Element : not null Program.Elements.Function_Calls
           .Function_Call_Access)
      is
      begin
         Self.Down (Element.Prefix, Self.Solution.Tuple (0));

         for J in Element.Parameters.Each_Element loop
            Self.Down (J.Element, Self.Solution.Tuple (J.Index));
         end loop;
      end Function_Call;

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
                 (Element.To_Element,
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
         Self.Down (Element.Operator, Self.Solution.Tuple (0));
         Self.Down (Element.Left,     Self.Solution.Tuple (1));
         Self.Down (Element.Right,    Self.Solution.Tuple (2));
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
                 (Element.To_Element,
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

      overriding procedure Record_Aggregate
        (Self    : in out Visitor;
         Element : not null Program.Elements.Record_Aggregates
                              .Record_Aggregate_Access)
      is
         View : constant Program.Visibility.View := Self.Solution.Type_View;
      begin
         for J in Element.Components.Each_Element loop
            declare
               Compon : Program.Visibility.View;

               Assoc  : constant Program.Elements.Record_Component_Associations
                 .Record_Component_Association_Access :=
                   J.Element.To_Record_Component_Association;

               Choices : constant Program.Element_Vectors
                 .Element_Vector_Access := Assoc.Choices;
            begin
               pragma Assert (Choices.Length > 0);

               for K in Choices.Each_Element loop
                  Program.Resolvers.Name_In_Region.Resolve_Name
                    (Region => View,
                     Name   => K.Element.To_Expression,
                     Setter => Self.Setter);

                  Compon := Self.Sets.Env.Get_Name_View (K.Element);

                  Resolve_To_Expected_Type
                    (Assoc.Component_Value.To_Element,
                     Self.Sets,
                     Self.Setter,
                     Expect => Program.Visibility.Subtype_Mark (Compon));
               end loop;
            end;
         end loop;
      end Record_Aggregate;

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
        .Cross_Reference_Updater_Access;
      Sets    : not null Program.Interpretations.Context_Access)
   is
      Down : Down_Visitors.Visitor := (Sets, Setter, False, Solution);
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
        Up (Element.To_Element, Sets);

      Found : Program.Interpretations.Solution;
      Count : Natural := 0;

   begin
      for N in Program.Interpretations.Expressions.Each (Set) loop
         Count := Count + 1;
         Found := -N;
         Result := +N;
      end loop;

      if Count > 0 then
         Down (Element.To_Element, Found, Setter, Sets);
      end if;
   end Resolve_To_Any_Type;

   ------------------------------
   -- Resolve_To_Discrete_Type --
   ------------------------------

   procedure Resolve_To_Discrete_Type
     (Element : not null Program.Elements.Expressions.Expression_Access;
      Sets    : not null Program.Interpretations.Context_Access;
      Setter  : not null Program.Cross_Reference_Updaters
                           .Cross_Reference_Updater_Access;
      Result  : out Program.Visibility.View)
   is
      use type Program.Interpretations.Expressions.Cursor;

      Set : constant Program.Interpretations.Interpretation_Set :=
        Up (Element, Sets);

      Found   : Program.Interpretations.Solution;
      Count   : Natural := 0;
      Matcher : Program.Type_Matchers.Discrete_Type_Matcher;

   begin
      for N in Program.Interpretations.Expressions.Each (Set) loop
         if Matcher.Is_Matched (+N) then
            Count := Count + 1;
            Found := -N;
            Result := +N;
         end if;
      end loop;

      if Count > 0 then
         Down (Element.To_Element, Found, Setter, Sets);
      end if;
   end Resolve_To_Discrete_Type;

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
         Down (Element.To_Element, Found, Setter, Sets);
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
