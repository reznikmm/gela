--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;

package Program.Elements.Operator_Symbols is

   pragma Pure (Program.Elements.Operator_Symbols);

   type Operator_Symbol is
     limited interface and Program.Elements.Expressions.Expression;

   type Operator_Symbol_Access is access all Operator_Symbol'Class
     with Storage_Size => 0;

   type Operator_Symbol_Text is limited interface;

   type Operator_Symbol_Text_Access is access all Operator_Symbol_Text'Class
     with Storage_Size => 0;

   not overriding function To_Operator_Symbol_Text
    (Self : aliased Operator_Symbol)
      return Operator_Symbol_Text_Access is abstract;

   not overriding function Operator_Symbol_Token
    (Self : Operator_Symbol_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Operator_Symbols;
