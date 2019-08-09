--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Elements.Operator_Symbols;
with Program.Elements.Infix_Operators;
with Program.Element_Visitors;

package Program.Nodes.Infix_Operators is

   pragma Preelaborate;

   type Infix_Operator is
     new Program.Nodes.Node and Program.Elements.Infix_Operators.Infix_Operator
         and Program.Elements.Infix_Operators.Infix_Operator_Text
     with private;

   function Create
    (Left     : Program.Elements.Expressions.Expression_Access;
     Operator : not null Program.Elements.Operator_Symbols
         .Operator_Symbol_Access;
     Right    : not null Program.Elements.Expressions.Expression_Access)
      return Infix_Operator;

   type Implicit_Infix_Operator is
     new Program.Nodes.Node and Program.Elements.Infix_Operators.Infix_Operator
     with private;

   function Create
    (Left                 : Program.Elements.Expressions.Expression_Access;
     Operator             : not null Program.Elements.Operator_Symbols
         .Operator_Symbol_Access;
     Right                : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Infix_Operator
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Infix_Operator is
     abstract new Program.Nodes.Node
       and Program.Elements.Infix_Operators.Infix_Operator
     with record
        Left     : Program.Elements.Expressions.Expression_Access;
        Operator : not null Program.Elements.Operator_Symbols
          .Operator_Symbol_Access;
        Right    : not null Program.Elements.Expressions.Expression_Access;
     end record;

   procedure Initialize (Self : aliased in out Base_Infix_Operator'Class);

   overriding procedure Visit
    (Self    : not null access Base_Infix_Operator;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Left
    (Self : Base_Infix_Operator)
      return Program.Elements.Expressions.Expression_Access;

   overriding function Operator
    (Self : Base_Infix_Operator)
      return not null Program.Elements.Operator_Symbols.Operator_Symbol_Access;

   overriding function Right
    (Self : Base_Infix_Operator)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Infix_Operator_Element
    (Self : Base_Infix_Operator)
      return Boolean;

   overriding function Is_Expression_Element
    (Self : Base_Infix_Operator)
      return Boolean;

   type Infix_Operator is
     new Base_Infix_Operator
       and Program.Elements.Infix_Operators.Infix_Operator_Text
     with null record;

   overriding function To_Infix_Operator_Text
    (Self : aliased in out Infix_Operator)
      return Program.Elements.Infix_Operators.Infix_Operator_Text_Access;

   type Implicit_Infix_Operator is
     new Base_Infix_Operator
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
     end record;

   overriding function To_Infix_Operator_Text
    (Self : aliased in out Implicit_Infix_Operator)
      return Program.Elements.Infix_Operators.Infix_Operator_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Infix_Operator)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Infix_Operator)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Infix_Operator)
      return Boolean;

end Program.Nodes.Infix_Operators;
