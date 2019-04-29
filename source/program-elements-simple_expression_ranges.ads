--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Constraints;
with Program.Elements.Discrete_Subtype_Definitions;
with Program.Elements.Discrete_Ranges;

package Program.Elements.Simple_Expression_Ranges is

   pragma Pure (Program.Elements.Simple_Expression_Ranges);

   type Simple_Expression_Range is
     limited interface and Program.Elements.Constraints.Constraint
       and Program.Elements.Discrete_Subtype_Definitions
         .Discrete_Subtype_Definition
       and Program.Elements.Discrete_Ranges.Discrete_Range;

   type Simple_Expression_Range_Access is
     access all Simple_Expression_Range'Class with Storage_Size => 0;

end Program.Elements.Simple_Expression_Ranges;
