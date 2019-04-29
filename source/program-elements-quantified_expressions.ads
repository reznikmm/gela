--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Quantified_Expressions is

   pragma Pure (Program.Elements.Quantified_Expressions);

   type Quantified_Expression is
     limited interface and Program.Elements.Expressions.Expression;

   type Quantified_Expression_Access is access all Quantified_Expression'Class
     with Storage_Size => 0;

end Program.Elements.Quantified_Expressions;
