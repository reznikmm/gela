--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.Simple_Return_Statements is

   pragma Pure (Program.Elements.Simple_Return_Statements);

   type Simple_Return_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Simple_Return_Statement_Access is
     access all Simple_Return_Statement'Class with Storage_Size => 0;

   not overriding function Return_Token
    (Self : Simple_Return_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Expression
    (Self : Simple_Return_Statement)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Simple_Return_Statement)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Simple_Return_Statements;
