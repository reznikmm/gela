--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;

package Program.Elements.Variant_Parts is

   pragma Pure (Program.Elements.Variant_Parts);

   type Variant_Part is
     limited interface and Program.Elements.Definitions.Definition;

   type Variant_Part_Access is access all Variant_Part'Class
     with Storage_Size => 0;

end Program.Elements.Variant_Parts;
