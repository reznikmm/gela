--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Elements.Discrete_Subtype_Definitions;
with Program.Elements.Discrete_Ranges;

package Program.Elements.Subtype_Indications is

   pragma Pure (Program.Elements.Subtype_Indications);

   type Subtype_Indication is
     limited interface and Program.Elements.Definitions.Definition
       and Program.Elements.Discrete_Subtype_Definitions
         .Discrete_Subtype_Definition
       and Program.Elements.Discrete_Ranges.Discrete_Range;

   type Subtype_Indication_Access is access all Subtype_Indication'Class
     with Storage_Size => 0;

end Program.Elements.Subtype_Indications;
