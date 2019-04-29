--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Explicit_Dereferences is

   pragma Pure (Program.Elements.Explicit_Dereferences);

   type Explicit_Dereference is
     limited interface and Program.Elements.Expressions.Expression;

   type Explicit_Dereference_Access is access all Explicit_Dereference'Class
     with Storage_Size => 0;

end Program.Elements.Explicit_Dereferences;
