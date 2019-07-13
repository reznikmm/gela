--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;

package Program.Elements.Type_Definitions is

   pragma Pure (Program.Elements.Type_Definitions);

   type Type_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Type_Definition_Access is access all Type_Definition'Class
     with Storage_Size => 0;

end Program.Elements.Type_Definitions;
