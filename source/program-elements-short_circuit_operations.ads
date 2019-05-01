--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;

package Program.Elements.Short_Circuit_Operations is

   pragma Pure (Program.Elements.Short_Circuit_Operations);

   type Short_Circuit_Operation is
     limited interface and Program.Elements.Expressions.Expression;

   type Short_Circuit_Operation_Access is
     access all Short_Circuit_Operation'Class with Storage_Size => 0;

   not overriding function Left_Expression
    (Self : Short_Circuit_Operation)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function And_Token
    (Self : Short_Circuit_Operation)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Then_Token
    (Self : Short_Circuit_Operation)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Or_Token
    (Self : Short_Circuit_Operation)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Else_Token
    (Self : Short_Circuit_Operation)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Right_Expression
    (Self : Short_Circuit_Operation)
      return Program.Elements.Expressions.Expression_Access is abstract;

end Program.Elements.Short_Circuit_Operations;
