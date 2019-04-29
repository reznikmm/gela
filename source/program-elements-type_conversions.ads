--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Type_Conversions is

   pragma Pure (Program.Elements.Type_Conversions);

   type Type_Conversion is
     limited interface and Program.Elements.Expressions.Expression;

   type Type_Conversion_Access is access all Type_Conversion'Class
     with Storage_Size => 0;

end Program.Elements.Type_Conversions;
