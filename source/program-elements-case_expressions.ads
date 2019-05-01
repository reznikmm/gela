--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;

package Program.Elements.Case_Expressions is

   pragma Pure (Program.Elements.Case_Expressions);

   type Case_Expression is
     limited interface and Program.Elements.Expressions.Expression;

   type Case_Expression_Access is access all Case_Expression'Class
     with Storage_Size => 0;

   not overriding function Case_Token
    (Self : Case_Expression)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Selecting_Expression
    (Self : Case_Expression)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Is_Token
    (Self : Case_Expression)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Case_Expressions;
