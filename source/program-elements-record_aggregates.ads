--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Record_Aggregates is

   pragma Pure (Program.Elements.Record_Aggregates);

   type Record_Aggregate is
     limited interface and Program.Elements.Expressions.Expression;

   type Record_Aggregate_Access is access all Record_Aggregate'Class
     with Storage_Size => 0;

end Program.Elements.Record_Aggregates;
