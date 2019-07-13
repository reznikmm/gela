--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;

package Program.Elements.Constraints is

   pragma Pure (Program.Elements.Constraints);

   type Constraint is
     limited interface and Program.Elements.Definitions.Definition;

   type Constraint_Access is access all Constraint'Class
     with Storage_Size => 0;

end Program.Elements.Constraints;
