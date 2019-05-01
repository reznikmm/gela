--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;

package Program.Elements.Character_Literals is

   pragma Pure (Program.Elements.Character_Literals);

   type Character_Literal is
     limited interface and Program.Elements.Expressions.Expression;

   type Character_Literal_Access is access all Character_Literal'Class
     with Storage_Size => 0;

   not overriding function Character_Literal_Token
    (Self : Character_Literal)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Character_Literals;
