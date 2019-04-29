--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;

package Program.Elements.Anonymous_Access_Definitions is

   pragma Pure (Program.Elements.Anonymous_Access_Definitions);

   type Anonymous_Access_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Anonymous_Access_Definition_Access is
     access all Anonymous_Access_Definition'Class with Storage_Size => 0;

end Program.Elements.Anonymous_Access_Definitions;
