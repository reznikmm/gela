--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;

package Program.Elements.Null_Literals is

   pragma Pure (Program.Elements.Null_Literals);

   type Null_Literal is
     limited interface and Program.Elements.Expressions.Expression;

   type Null_Literal_Access is access all Null_Literal'Class
     with Storage_Size => 0;

   not overriding function Null_Literal_Token
    (Self : Null_Literal)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Null_Literals;
