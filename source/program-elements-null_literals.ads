--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Null_Literals is

   pragma Pure (Program.Elements.Null_Literals);

   type Null_Literal is
     limited interface and Program.Elements.Expressions.Expression;

   type Null_Literal_Access is access all Null_Literal'Class
     with Storage_Size => 0;

end Program.Elements.Null_Literals;
