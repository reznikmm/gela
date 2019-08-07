--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;

package Program.Elements.String_Literals is

   pragma Pure (Program.Elements.String_Literals);

   type String_Literal is
     limited interface and Program.Elements.Expressions.Expression;

   type String_Literal_Access is access all String_Literal'Class
     with Storage_Size => 0;

   not overriding function Image (Self : String_Literal) return Text
     is abstract;

   type String_Literal_Text is limited interface;

   type String_Literal_Text_Access is access all String_Literal_Text'Class
     with Storage_Size => 0;

   not overriding function To_String_Literal_Text
    (Self : aliased in out String_Literal)
      return String_Literal_Text_Access is abstract;

   not overriding function String_Literal_Token
    (Self : String_Literal_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.String_Literals;
