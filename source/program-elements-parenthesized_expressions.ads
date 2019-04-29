--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Parenthesized_Expressions is

   pragma Pure (Program.Elements.Parenthesized_Expressions);

   type Parenthesized_Expression is
     limited interface and Program.Elements.Expressions.Expression;

   type Parenthesized_Expression_Access is
     access all Parenthesized_Expression'Class with Storage_Size => 0;

end Program.Elements.Parenthesized_Expressions;