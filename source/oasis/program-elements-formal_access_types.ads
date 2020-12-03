--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Formal_Type_Definitions;

package Program.Elements.Formal_Access_Types is

   pragma Pure (Program.Elements.Formal_Access_Types);

   type Formal_Access_Type is
     limited interface
       and Program.Elements.Formal_Type_Definitions.Formal_Type_Definition;

   type Formal_Access_Type_Access is access all Formal_Access_Type'Class
     with Storage_Size => 0;

end Program.Elements.Formal_Access_Types;
