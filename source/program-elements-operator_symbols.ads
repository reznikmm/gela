--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Operator_Symbols is

   pragma Pure (Program.Elements.Operator_Symbols);

   type Operator_Symbol is
     limited interface and Program.Elements.Expressions.Expression;

   type Operator_Symbol_Access is access all Operator_Symbol'Class
     with Storage_Size => 0;

end Program.Elements.Operator_Symbols;
