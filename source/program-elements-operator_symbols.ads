--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;

package Program.Elements.Operator_Symbols is

   pragma Pure (Program.Elements.Operator_Symbols);

   type Operator_Symbol is
     limited interface and Program.Elements.Expressions.Expression;

   type Operator_Symbol_Access is access all Operator_Symbol'Class
     with Storage_Size => 0;

   not overriding function Operator_Symbol_Token
    (Self : Operator_Symbol)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Operator_Symbols;
