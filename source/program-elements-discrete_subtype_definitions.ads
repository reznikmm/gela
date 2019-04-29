--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;

package Program.Elements.Discrete_Subtype_Definitions is

   pragma Pure (Program.Elements.Discrete_Subtype_Definitions);

   type Discrete_Subtype_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Discrete_Subtype_Definition_Access is
     access all Discrete_Subtype_Definition'Class with Storage_Size => 0;

end Program.Elements.Discrete_Subtype_Definitions;
