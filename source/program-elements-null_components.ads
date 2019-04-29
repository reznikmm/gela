--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;

package Program.Elements.Null_Components is

   pragma Pure (Program.Elements.Null_Components);

   type Null_Component is
     limited interface and Program.Elements.Definitions.Definition;

   type Null_Component_Access is access all Null_Component'Class
     with Storage_Size => 0;

end Program.Elements.Null_Components;
