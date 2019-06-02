--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.Delay_Statements is

   pragma Pure (Program.Elements.Delay_Statements);

   type Delay_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Delay_Statement_Access is access all Delay_Statement'Class
     with Storage_Size => 0;

   not overriding function Delay_Token
    (Self : Delay_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Until_Token
    (Self : Delay_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Expression
    (Self : Delay_Statement)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Delay_Statement)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Delay_Statements;
