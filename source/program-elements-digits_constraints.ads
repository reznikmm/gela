--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Constraints;

package Program.Elements.Digits_Constraints is

   pragma Pure (Program.Elements.Digits_Constraints);

   type Digits_Constraint is
     limited interface and Program.Elements.Constraints.Constraint;

   type Digits_Constraint_Access is access all Digits_Constraint'Class
     with Storage_Size => 0;

end Program.Elements.Digits_Constraints;
