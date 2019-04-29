--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Function_Calls is

   pragma Pure (Program.Elements.Function_Calls);

   type Function_Call is
     limited interface and Program.Elements.Expressions.Expression;

   type Function_Call_Access is access all Function_Call'Class
     with Storage_Size => 0;

end Program.Elements.Function_Calls;
