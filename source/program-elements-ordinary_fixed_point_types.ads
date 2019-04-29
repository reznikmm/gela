--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;

package Program.Elements.Ordinary_Fixed_Point_Types is

   pragma Pure (Program.Elements.Ordinary_Fixed_Point_Types);

   type Ordinary_Fixed_Point_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Ordinary_Fixed_Point_Type_Access is
     access all Ordinary_Fixed_Point_Type'Class with Storage_Size => 0;

end Program.Elements.Ordinary_Fixed_Point_Types;
