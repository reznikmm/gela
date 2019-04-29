--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Constraints;

package Program.Elements.Delta_Constraints is

   pragma Pure (Program.Elements.Delta_Constraints);

   type Delta_Constraint is
     limited interface and Program.Elements.Constraints.Constraint;

   type Delta_Constraint_Access is access all Delta_Constraint'Class
     with Storage_Size => 0;

end Program.Elements.Delta_Constraints;
