--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Lexical_Elements;

package Program.Elements.Parenthesized_Expressions is

   pragma Pure (Program.Elements.Parenthesized_Expressions);

   type Parenthesized_Expression is
     limited interface and Program.Elements.Expressions.Expression;

   type Parenthesized_Expression_Access is
     access all Parenthesized_Expression'Class with Storage_Size => 0;

   not overriding function Expression
    (Self : Parenthesized_Expression)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Parenthesized_Expression_Text is limited interface;

   type Parenthesized_Expression_Text_Access is
     access all Parenthesized_Expression_Text'Class with Storage_Size => 0;

   not overriding function To_Parenthesized_Expression_Text
    (Self : aliased in out Parenthesized_Expression)
      return Parenthesized_Expression_Text_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Parenthesized_Expression_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Right_Bracket_Token
    (Self : Parenthesized_Expression_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Parenthesized_Expressions;
