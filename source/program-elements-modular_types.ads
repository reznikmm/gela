--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;

package Program.Elements.Modular_Types is

   pragma Pure (Program.Elements.Modular_Types);

   type Modular_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Modular_Type_Access is access all Modular_Type'Class
     with Storage_Size => 0;

end Program.Elements.Modular_Types;
