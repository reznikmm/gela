--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;

package Program.Elements.Variants is

   pragma Pure (Program.Elements.Variants);

   type Variant is
     limited interface and Program.Elements.Definitions.Definition;

   type Variant_Access is access all Variant'Class with Storage_Size => 0;

end Program.Elements.Variants;
