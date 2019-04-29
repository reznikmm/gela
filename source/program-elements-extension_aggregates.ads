--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Extension_Aggregates is

   pragma Pure (Program.Elements.Extension_Aggregates);

   type Extension_Aggregate is
     limited interface and Program.Elements.Expressions.Expression;

   type Extension_Aggregate_Access is access all Extension_Aggregate'Class
     with Storage_Size => 0;

end Program.Elements.Extension_Aggregates;
