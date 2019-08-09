--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Infix_Operators is

   function Create
    (Left     : Program.Elements.Expressions.Expression_Access;
     Operator : not null Program.Elements.Operator_Symbols
         .Operator_Symbol_Access;
     Right    : not null Program.Elements.Expressions.Expression_Access)
      return Infix_Operator is
   begin
      return Result : Infix_Operator :=
        (Left => Left, Operator => Operator, Right => Right,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Left                 : Program.Elements.Expressions.Expression_Access;
     Operator             : not null Program.Elements.Operator_Symbols
         .Operator_Symbol_Access;
     Right                : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Infix_Operator is
   begin
      return Result : Implicit_Infix_Operator :=
        (Left => Left, Operator => Operator, Right => Right,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Left
    (Self : Base_Infix_Operator)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Left;
   end Left;

   overriding function Operator
    (Self : Base_Infix_Operator)
      return not null Program.Elements.Operator_Symbols
          .Operator_Symbol_Access is
   begin
      return Self.Operator;
   end Operator;

   overriding function Right
    (Self : Base_Infix_Operator)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Right;
   end Right;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Infix_Operator)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Infix_Operator)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Infix_Operator)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Infix_Operator'Class) is
   begin
      if Self.Left.Assigned then
         Set_Enclosing_Element (Self.Left, Self'Unchecked_Access);
      end if;
      Set_Enclosing_Element (Self.Operator, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Right, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Infix_Operator
    (Self : Base_Infix_Operator)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Infix_Operator;

   overriding function Is_Expression
    (Self : Base_Infix_Operator)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression;

   overriding procedure Visit
    (Self    : not null access Base_Infix_Operator;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Infix_Operator (Self);
   end Visit;

   overriding function To_Infix_Operator_Text
    (Self : aliased in out Infix_Operator)
      return Program.Elements.Infix_Operators.Infix_Operator_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Infix_Operator_Text;

   overriding function To_Infix_Operator_Text
    (Self : aliased in out Implicit_Infix_Operator)
      return Program.Elements.Infix_Operators.Infix_Operator_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Infix_Operator_Text;

end Program.Nodes.Infix_Operators;
