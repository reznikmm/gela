--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;

package Program.Elements.Discrete_Ranges is

   pragma Pure (Program.Elements.Discrete_Ranges);

   type Discrete_Range is
     limited interface and Program.Elements.Definitions.Definition;

   type Discrete_Range_Access is access all Discrete_Range'Class
     with Storage_Size => 0;

end Program.Elements.Discrete_Ranges;
