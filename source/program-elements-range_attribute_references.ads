--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Constraints;
with Program.Elements.Discrete_Subtype_Definitions;
with Program.Elements.Discrete_Ranges;

package Program.Elements.Range_Attribute_References is

   pragma Pure (Program.Elements.Range_Attribute_References);

   type Range_Attribute_Reference is
     limited interface and Program.Elements.Constraints.Constraint
       and Program.Elements.Discrete_Subtype_Definitions
         .Discrete_Subtype_Definition
       and Program.Elements.Discrete_Ranges.Discrete_Range;

   type Range_Attribute_Reference_Access is
     access all Range_Attribute_Reference'Class with Storage_Size => 0;

end Program.Elements.Range_Attribute_References;
