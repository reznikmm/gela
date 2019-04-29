--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Numeric_Literals is

   pragma Pure (Program.Elements.Numeric_Literals);

   type Numeric_Literal is
     limited interface and Program.Elements.Expressions.Expression;

   type Numeric_Literal_Access is access all Numeric_Literal'Class
     with Storage_Size => 0;

end Program.Elements.Numeric_Literals;
