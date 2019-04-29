--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;

package Program.Elements.Derived_Types is

   pragma Pure (Program.Elements.Derived_Types);

   type Derived_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Derived_Type_Access is access all Derived_Type'Class
     with Storage_Size => 0;

end Program.Elements.Derived_Types;
