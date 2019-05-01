--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;

package Program.Elements.Box_Expressions is

   pragma Pure (Program.Elements.Box_Expressions);

   type Box_Expression is
     limited interface and Program.Elements.Expressions.Expression;

   type Box_Expression_Access is access all Box_Expression'Class
     with Storage_Size => 0;

   not overriding function Box_Token
    (Self : Box_Expression)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Box_Expressions;
