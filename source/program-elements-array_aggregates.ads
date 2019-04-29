--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Array_Aggregates is

   pragma Pure (Program.Elements.Array_Aggregates);

   type Array_Aggregate is
     limited interface and Program.Elements.Expressions.Expression;

   type Array_Aggregate_Access is access all Array_Aggregate'Class
     with Storage_Size => 0;

end Program.Elements.Array_Aggregates;
