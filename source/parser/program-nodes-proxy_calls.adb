--  SPDX-FileCopyrightText: 2020-2021 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Nodes.Proxy_Associations;

package body Program.Nodes.Proxy_Calls is

   -----------------
   -- Called_Name --
   -----------------

   overriding function Called_Name (Self : Proxy_Call)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Called_Name;
   end Called_Name;

   ----------------
   -- Components --
   ----------------

   overriding function Components (Self : Proxy_Call)
      return Program.Elements.Record_Component_Associations
          .Record_Component_Association_Vector_Access is
   begin
      return Program.Elements.Record_Component_Associations
        .Record_Component_Association_Vector_Access
          (Self.Components);
   end Components;

   ------------
   -- Create --
   ------------

   function Create
    (Called_Name         : Program.Elements.Expressions.Expression_Access;
     Left_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters          : Program.Element_Vectors.Element_Vector_Access;
     Right_Bracket_Token : Program.Lexical_Elements.Lexical_Element_Access)
      return Proxy_Call is
   begin
      pragma Warnings (Off, "others choice is redundant");  --  gpl 2019
      return Self : aliased Proxy_Call :=
        (This                => <>,
         Current             => A_Record_Aggregate,
         Called_Name         => Called_Name,
         Left_Bracket_Token  => Left_Bracket_Token,
         Components          => Parameters,
         Parameters          => <>,
         Discr               => <>,
         Ranges              => <>,
         Right_Bracket_Token => Right_Bracket_Token,
         Semicolon_Token     => null,
         Enclosing_Element   => null,
         Text => <>)
      do
         if Called_Name.Assigned then
            Set_Enclosing_Element (Self.Called_Name, Self'Unchecked_Access);
         end if;

         for Item in Self.Components.Each_Element loop
            Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
         end loop;
      end return;
   end Create;

   ---------------
   -- Delimiter --
   ---------------

   overriding function Delimiter
     (Self  : Base_Vector;
      Index : Positive)
     return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Parent.Components.Delimiter (Index);
   end Delimiter;

   -------------------
   -- Discriminants --
   -------------------

   overriding function Discriminants (Self : Proxy_Call)
     return not null Program.Elements.Discriminant_Associations
       .Discriminant_Association_Vector_Access is
   begin
      return Self.This.Discr'Unchecked_Access;
   end Discriminants;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self  : Parameter_Vector;
      Index : Positive)
      return not null Program.Elements.Element_Access is
   begin
      return Self.Parent.Components.Element (Index);
   end Element;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self  : Discriminant_Association_Vector;
      Index : Positive)
     return not null Program.Elements.Element_Access is
   begin
      return Self.Parent.Components.Element (Index);
   end Element;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self  : Discrete_Range_Vector;
      Index : Positive)
      return not null Program.Elements.Element_Access
   is
      Result : not null Program.Elements.Element_Access :=
        Self.Parent.Components.Element (Index);
   begin
      if Result.Is_Discrete_Simple_Expression_Range then
         Result := Program.Nodes.Proxy_Associations.Proxy_Association_Access
           (Result).Choices.Element (1);
      end if;

      return Result;
   end Element;

   ----------------
   -- Get_Length --
   ----------------

   overriding function Get_Length (Self : Base_Vector) return Positive is
   begin
      return Self.Parent.Components.Get_Length;
   end Get_Length;

   -----------------------
   -- Is_Call_Statement --
   -----------------------

   overriding function Is_Call_Statement (Self : Proxy_Call) return Boolean is
   begin
      return Self.Current = A_Call_Statement;
   end Is_Call_Statement;

   -------------------
   -- Is_Constraint --
   -------------------

   overriding function Is_Constraint (Self : Proxy_Call) return Boolean is
   begin
      return Self.Current in A_Discriminant_Constraint | An_Index_Constraint;
   end Is_Constraint;

   -------------------
   -- Is_Definition --
   -------------------

   overriding function Is_Definition (Self : Proxy_Call) return Boolean is
   begin
      return Self.Current in A_Discriminant_Constraint | An_Index_Constraint;
   end Is_Definition;

   --------------------------------
   -- Is_Discriminant_Constraint --
   --------------------------------

   overriding function Is_Discriminant_Constraint (Self : Proxy_Call)
      return Boolean is
   begin
      return Self.Current = A_Discriminant_Constraint;
   end Is_Discriminant_Constraint;

   -------------------
   -- Is_Expression --
   -------------------

   overriding function Is_Expression (Self : Proxy_Call) return Boolean is
   begin
      return Self.Current in A_Record_Aggregate | A_Function_Call;
   end Is_Expression;

   ----------------------
   -- Is_Function_Call --
   ----------------------

   overriding function Is_Function_Call (Self : Proxy_Call)
      return Boolean is
   begin
      return Self.Current = A_Function_Call;
   end Is_Function_Call;

   -------------------------
   -- Is_Index_Constraint --
   -------------------------

   overriding function Is_Index_Constraint (Self : Proxy_Call)
     return Boolean is
   begin
      return Self.Current = An_Index_Constraint;
   end Is_Index_Constraint;

   -------------------------
   -- Is_Record_Aggregate --
   -------------------------

   overriding function Is_Record_Aggregate (Self : Proxy_Call)
     return Boolean is
   begin
      return Self.Current = A_Record_Aggregate;
   end Is_Record_Aggregate;

   ------------------
   -- Is_Statement --
   ------------------

   overriding function Is_Statement (Self : Proxy_Call) return Boolean is
   begin
      return Self.Current = A_Call_Statement;
   end Is_Statement;

   ------------------------
   -- Left_Bracket_Token --
   ------------------------

   overriding function Left_Bracket_Token (Self : Proxy_Call)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   ------------------------
   -- Left_Bracket_Token --
   ------------------------

   overriding function Left_Bracket_Token (Self : Proxy_Call_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Parent.Left_Bracket_Token;
   end Left_Bracket_Token;

   ----------------
   -- Parameters --
   ----------------

   overriding function Parameters (Self : Proxy_Call)
      return Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access is
   begin
      return Self.This.Parameters'Unchecked_Access;
   end Parameters;

   ------------
   -- Prefix --
   ------------

   overriding function Prefix (Self : Proxy_Call)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Called_Name;
   end Prefix;

   ------------
   -- Ranges --
   ------------

   overriding function Ranges (Self : Proxy_Call)
     return not null Program.Elements.Discrete_Ranges
       .Discrete_Range_Vector_Access is
   begin
      return Self.This.Ranges'Unchecked_Access;
   end Ranges;

   -------------------------
   -- Right_Bracket_Token --
   -------------------------

   overriding function Right_Bracket_Token (Self : Proxy_Call)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   -------------------------
   -- Right_Bracket_Token --
   -------------------------

   overriding function Right_Bracket_Token (Self : Proxy_Call_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Parent.Right_Bracket_Token;
   end Right_Bracket_Token;

   ---------------------
   -- Semicolon_Token --
   ---------------------

   overriding function Semicolon_Token (Self : Proxy_Call)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   ----------------------------
   -- To_Call_Statement_Text --
   ----------------------------

   overriding function To_Call_Statement_Text
    (Self : aliased in out Proxy_Call)
      return Program.Elements.Call_Statements.Call_Statement_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Call_Statement_Text;

   -------------------------------------
   -- To_Discriminant_Constraint_Text --
   -------------------------------------

   overriding function To_Discriminant_Constraint_Text
     (Self : aliased in out Proxy_Call)
      return Program.Elements.Discriminant_Constraints
        .Discriminant_Constraint_Text_Access is
   begin
      return Self.Text'Unchecked_Access;
   end To_Discriminant_Constraint_Text;

   ---------------------------
   -- To_Function_Call_Text --
   ---------------------------

   overriding function To_Function_Call_Text (Self : aliased in out Proxy_Call)
      return Program.Elements.Function_Calls.Function_Call_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Function_Call_Text;

   ------------------------------
   -- To_Index_Constraint_Text --
   ------------------------------

   overriding function To_Index_Constraint_Text
     (Self : aliased in out Proxy_Call)
      return Program.Elements.Index_Constraints.Index_Constraint_Text_Access is
   begin
      return Self.Text'Unchecked_Access;
   end To_Index_Constraint_Text;

   ------------------------------
   -- To_Record_Aggregate_Text --
   ------------------------------

   overriding function To_Record_Aggregate_Text
     (Self : aliased in out Proxy_Call)
      return Program.Elements.Record_Aggregates.Record_Aggregate_Text_Access is
   begin
      return Self.Text'Unchecked_Access;
   end To_Record_Aggregate_Text;

   -------------------------------------
   -- Turn_To_Discriminant_Constraint --
   -------------------------------------

   procedure Turn_To_Discriminant_Constraint
     (Self : in out Proxy_Call'Class;
      Mark : out Program.Elements.Expressions.Expression_Access) is
   begin
      Self.Current := A_Discriminant_Constraint;
      Mark := Self.Called_Name;

      for Item in Self.Components.Each_Element loop
         Program.Nodes.Proxy_Associations.Proxy_Association_Access
           (Item.Element).Turn_To_Discriminant_Association;
      end loop;
   end Turn_To_Discriminant_Constraint;

   ---------------------------
   -- Turn_To_Function_Call --
   ---------------------------

   procedure Turn_To_Function_Call
     (Self        : in out Proxy_Call'Class;
      Called_Name : not null Program.Elements.Expressions.Expression_Access) is
   begin
      Self.Current := A_Call_Statement;
      Self.Called_Name := Called_Name;

      for Item in Self.Components.Each_Element loop
         Program.Nodes.Proxy_Associations.Proxy_Association_Access
           (Item.Element).Turn_To_Parameter;
      end loop;
   end Turn_To_Function_Call;

   ------------------------------
   -- Turn_To_Index_Constraint --
   ------------------------------

   procedure Turn_To_Index_Constraint (Self : in out Proxy_Call'Class) is
   begin
      Self.Current := An_Index_Constraint;

      for Item in Self.Components.Each_Element loop
         Program.Nodes.Proxy_Associations.Proxy_Association_Access
           (Item.Element).Turn_To_Discrete_Range;
      end loop;
   end Turn_To_Index_Constraint;

   ----------------------------
   -- Turn_To_Procedure_Call --
   ----------------------------

   procedure Turn_To_Procedure_Call
     (Self            : in out Proxy_Call'Class;
      Semicolon_Token : Program.Lexical_Elements.Lexical_Element_Access) is
   begin
      Self.Current := A_Call_Statement;
      Self.Semicolon_Token := Semicolon_Token;
   end Turn_To_Procedure_Call;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
    (Self    : not null access Proxy_Call;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      case Self.Current is
         when A_Call_Statement =>
            Visitor.Call_Statement (Self);
         when A_Function_Call =>
            Visitor.Function_Call (Self);
         when A_Discriminant_Constraint =>
            Visitor.Discriminant_Constraint (Self);
         when An_Index_Constraint =>
            Visitor.Index_Constraint (Self);
         when A_Record_Aggregate =>
            Visitor.Record_Aggregate (Self);
      end case;
   end Visit;

end Program.Nodes.Proxy_Calls;
