--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.String_Literals is

   pragma Pure (Program.Elements.String_Literals);

   type String_Literal is
     limited interface and Program.Elements.Expressions.Expression;

   type String_Literal_Access is access all String_Literal'Class
     with Storage_Size => 0;

end Program.Elements.String_Literals;
