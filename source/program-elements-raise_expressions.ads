--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Raise_Expressions is

   pragma Pure (Program.Elements.Raise_Expressions);

   type Raise_Expression is
     limited interface and Program.Elements.Expressions.Expression;

   type Raise_Expression_Access is access all Raise_Expression'Class
     with Storage_Size => 0;

end Program.Elements.Raise_Expressions;