--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;

package Program.Elements.Numeric_Literals is

   pragma Pure (Program.Elements.Numeric_Literals);

   type Numeric_Literal is
     limited interface and Program.Elements.Expressions.Expression;

   type Numeric_Literal_Access is access all Numeric_Literal'Class
     with Storage_Size => 0;

   not overriding function Numeric_Literal_Token
    (Self : Numeric_Literal)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Numeric_Literals;
