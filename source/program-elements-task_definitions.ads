--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;

package Program.Elements.Task_Definitions is

   pragma Pure (Program.Elements.Task_Definitions);

   type Task_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Task_Definition_Access is access all Task_Definition'Class
     with Storage_Size => 0;

end Program.Elements.Task_Definitions;
