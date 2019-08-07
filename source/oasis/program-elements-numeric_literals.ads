--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;

package Program.Elements.Numeric_Literals is

   pragma Pure (Program.Elements.Numeric_Literals);

   type Numeric_Literal is
     limited interface and Program.Elements.Expressions.Expression;

   type Numeric_Literal_Access is access all Numeric_Literal'Class
     with Storage_Size => 0;

   not overriding function Image (Self : Numeric_Literal) return Text
     is abstract;

   type Numeric_Literal_Text is limited interface;

   type Numeric_Literal_Text_Access is access all Numeric_Literal_Text'Class
     with Storage_Size => 0;

   not overriding function To_Numeric_Literal_Text
    (Self : aliased in out Numeric_Literal)
      return Numeric_Literal_Text_Access is abstract;

   not overriding function Numeric_Literal_Token
    (Self : Numeric_Literal_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Numeric_Literals;
