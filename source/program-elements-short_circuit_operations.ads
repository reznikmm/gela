--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Short_Circuit_Operations is

   pragma Pure (Program.Elements.Short_Circuit_Operations);

   type Short_Circuit_Operation is
     limited interface and Program.Elements.Expressions.Expression;

   type Short_Circuit_Operation_Access is
     access all Short_Circuit_Operation'Class with Storage_Size => 0;

end Program.Elements.Short_Circuit_Operations;
