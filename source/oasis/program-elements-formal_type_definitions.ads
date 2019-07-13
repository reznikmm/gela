--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;

package Program.Elements.Formal_Type_Definitions is

   pragma Pure (Program.Elements.Formal_Type_Definitions);

   type Formal_Type_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Formal_Type_Definition_Access is
     access all Formal_Type_Definition'Class with Storage_Size => 0;

end Program.Elements.Formal_Type_Definitions;
