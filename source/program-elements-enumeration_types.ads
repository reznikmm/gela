--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;

package Program.Elements.Enumeration_Types is

   pragma Pure (Program.Elements.Enumeration_Types);

   type Enumeration_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Enumeration_Type_Access is access all Enumeration_Type'Class
     with Storage_Size => 0;

end Program.Elements.Enumeration_Types;
