--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;

package Program.Elements.Private_Type_Definitions is

   pragma Pure (Program.Elements.Private_Type_Definitions);

   type Private_Type_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Private_Type_Definition_Access is
     access all Private_Type_Definition'Class with Storage_Size => 0;

end Program.Elements.Private_Type_Definitions;
