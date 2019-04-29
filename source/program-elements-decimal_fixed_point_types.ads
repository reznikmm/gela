--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;

package Program.Elements.Decimal_Fixed_Point_Types is

   pragma Pure (Program.Elements.Decimal_Fixed_Point_Types);

   type Decimal_Fixed_Point_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Decimal_Fixed_Point_Type_Access is
     access all Decimal_Fixed_Point_Type'Class with Storage_Size => 0;

end Program.Elements.Decimal_Fixed_Point_Types;
