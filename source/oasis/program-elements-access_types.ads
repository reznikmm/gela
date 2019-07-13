--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;

package Program.Elements.Access_Types is

   pragma Pure (Program.Elements.Access_Types);

   type Access_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Access_Type_Access is access all Access_Type'Class
     with Storage_Size => 0;

end Program.Elements.Access_Types;
