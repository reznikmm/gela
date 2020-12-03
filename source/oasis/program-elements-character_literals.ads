--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;

package Program.Elements.Character_Literals is

   pragma Pure (Program.Elements.Character_Literals);

   type Character_Literal is
     limited interface and Program.Elements.Expressions.Expression;

   type Character_Literal_Access is access all Character_Literal'Class
     with Storage_Size => 0;

   not overriding function Image (Self : Character_Literal) return Text
     is abstract;

   type Character_Literal_Text is limited interface;

   type Character_Literal_Text_Access is
     access all Character_Literal_Text'Class with Storage_Size => 0;

   not overriding function To_Character_Literal_Text
    (Self : aliased in out Character_Literal)
      return Character_Literal_Text_Access is abstract;

   not overriding function Character_Literal_Token
    (Self : Character_Literal_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Character_Literals;
