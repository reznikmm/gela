--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;

package Program.Elements.Floating_Point_Types is

   pragma Pure (Program.Elements.Floating_Point_Types);

   type Floating_Point_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Floating_Point_Type_Access is access all Floating_Point_Type'Class
     with Storage_Size => 0;

end Program.Elements.Floating_Point_Types;
