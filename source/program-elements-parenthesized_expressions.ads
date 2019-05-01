--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;

package Program.Elements.Parenthesized_Expressions is

   pragma Pure (Program.Elements.Parenthesized_Expressions);

   type Parenthesized_Expression is
     limited interface and Program.Elements.Expressions.Expression;

   type Parenthesized_Expression_Access is
     access all Parenthesized_Expression'Class with Storage_Size => 0;

   not overriding function Left_Bracket_Token
    (Self : Parenthesized_Expression)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Expression_Parenthesized
    (Self : Parenthesized_Expression)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Parenthesized_Expression)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Parenthesized_Expressions;
