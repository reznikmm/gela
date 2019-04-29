--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Attribute_References is

   pragma Pure (Program.Elements.Attribute_References);

   type Attribute_Reference is
     limited interface and Program.Elements.Expressions.Expression;

   type Attribute_Reference_Access is access all Attribute_Reference'Class
     with Storage_Size => 0;

end Program.Elements.Attribute_References;
