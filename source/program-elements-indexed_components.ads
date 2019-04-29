--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Indexed_Components is

   pragma Pure (Program.Elements.Indexed_Components);

   type Indexed_Component is
     limited interface and Program.Elements.Expressions.Expression;

   type Indexed_Component_Access is access all Indexed_Component'Class
     with Storage_Size => 0;

end Program.Elements.Indexed_Components;
