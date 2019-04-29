--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Qualified_Expressions is

   pragma Pure (Program.Elements.Qualified_Expressions);

   type Qualified_Expression is
     limited interface and Program.Elements.Expressions.Expression;

   type Qualified_Expression_Access is access all Qualified_Expression'Class
     with Storage_Size => 0;

end Program.Elements.Qualified_Expressions;
