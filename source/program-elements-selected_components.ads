--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Selected_Components is

   pragma Pure (Program.Elements.Selected_Components);

   type Selected_Component is
     limited interface and Program.Elements.Expressions.Expression;

   type Selected_Component_Access is access all Selected_Component'Class
     with Storage_Size => 0;

end Program.Elements.Selected_Components;
