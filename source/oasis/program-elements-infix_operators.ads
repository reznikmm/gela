--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Elements.Operator_Symbols;

package Program.Elements.Infix_Operators is

   pragma Pure (Program.Elements.Infix_Operators);

   type Infix_Operator is
     limited interface and Program.Elements.Expressions.Expression;

   type Infix_Operator_Access is access all Infix_Operator'Class
     with Storage_Size => 0;

   not overriding function Left
    (Self : Infix_Operator)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Operator
    (Self : Infix_Operator)
      return not null Program.Elements.Operator_Symbols.Operator_Symbol_Access
     is abstract;

   not overriding function Right
    (Self : Infix_Operator)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Infix_Operator_Text is limited interface;

   type Infix_Operator_Text_Access is access all Infix_Operator_Text'Class
     with Storage_Size => 0;

   not overriding function To_Infix_Operator_Text
    (Self : aliased in out Infix_Operator)
      return Infix_Operator_Text_Access is abstract;

end Program.Elements.Infix_Operators;
