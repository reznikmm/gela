--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;

package Program.Elements.Known_Discriminant_Parts is

   pragma Pure (Program.Elements.Known_Discriminant_Parts);

   type Known_Discriminant_Part is
     limited interface and Program.Elements.Definitions.Definition;

   type Known_Discriminant_Part_Access is
     access all Known_Discriminant_Part'Class with Storage_Size => 0;

end Program.Elements.Known_Discriminant_Parts;
