--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Box_Expressions is

   pragma Pure (Program.Elements.Box_Expressions);

   type Box_Expression is
     limited interface and Program.Elements.Expressions.Expression;

   type Box_Expression_Access is access all Box_Expression'Class
     with Storage_Size => 0;

end Program.Elements.Box_Expressions;
