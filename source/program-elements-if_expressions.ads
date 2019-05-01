--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;

package Program.Elements.If_Expressions is

   pragma Pure (Program.Elements.If_Expressions);

   type If_Expression is
     limited interface and Program.Elements.Expressions.Expression;

   type If_Expression_Access is access all If_Expression'Class
     with Storage_Size => 0;

   not overriding function If_Token
    (Self : If_Expression)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Condition_Expression
    (Self : If_Expression)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Then_Token
    (Self : If_Expression)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Then_Expression
    (Self : If_Expression)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Else_Token
    (Self : If_Expression)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Else_Expression
    (Self : If_Expression)
      return Program.Elements.Expressions.Expression_Access is abstract;

end Program.Elements.If_Expressions;
