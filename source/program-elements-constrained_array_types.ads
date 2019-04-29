--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;
with Program.Elements.Formal_Type_Definitions;

package Program.Elements.Constrained_Array_Types is

   pragma Pure (Program.Elements.Constrained_Array_Types);

   type Constrained_Array_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition
       and Program.Elements.Formal_Type_Definitions.Formal_Type_Definition;

   type Constrained_Array_Type_Access is
     access all Constrained_Array_Type'Class with Storage_Size => 0;

end Program.Elements.Constrained_Array_Types;
