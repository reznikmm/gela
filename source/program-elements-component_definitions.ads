--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;

package Program.Elements.Component_Definitions is

   pragma Pure (Program.Elements.Component_Definitions);

   type Component_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Component_Definition_Access is access all Component_Definition'Class
     with Storage_Size => 0;

end Program.Elements.Component_Definitions;
