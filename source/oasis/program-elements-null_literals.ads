--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;

package Program.Elements.Null_Literals is

   pragma Pure (Program.Elements.Null_Literals);

   type Null_Literal is
     limited interface and Program.Elements.Expressions.Expression;

   type Null_Literal_Access is access all Null_Literal'Class
     with Storage_Size => 0;

   type Null_Literal_Text is limited interface;

   type Null_Literal_Text_Access is access all Null_Literal_Text'Class
     with Storage_Size => 0;

   not overriding function To_Null_Literal_Text
    (Self : aliased in out Null_Literal)
      return Null_Literal_Text_Access is abstract;

   not overriding function Null_Literal_Token
    (Self : Null_Literal_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Null_Literals;
