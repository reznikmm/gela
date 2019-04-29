--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.If_Expressions is

   pragma Pure (Program.Elements.If_Expressions);

   type If_Expression is
     limited interface and Program.Elements.Expressions.Expression;

   type If_Expression_Access is access all If_Expression'Class
     with Storage_Size => 0;

end Program.Elements.If_Expressions;
