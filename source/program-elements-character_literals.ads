--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Character_Literals is

   pragma Pure (Program.Elements.Character_Literals);

   type Character_Literal is
     limited interface and Program.Elements.Expressions.Expression;

   type Character_Literal_Access is access all Character_Literal'Class
     with Storage_Size => 0;

end Program.Elements.Character_Literals;
