--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Constraints;

package Program.Elements.Discriminant_Constraints is

   pragma Pure (Program.Elements.Discriminant_Constraints);

   type Discriminant_Constraint is
     limited interface and Program.Elements.Constraints.Constraint;

   type Discriminant_Constraint_Access is
     access all Discriminant_Constraint'Class with Storage_Size => 0;

end Program.Elements.Discriminant_Constraints;
