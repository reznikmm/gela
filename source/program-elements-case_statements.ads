--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.Case_Statements is

   pragma Pure (Program.Elements.Case_Statements);

   type Case_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Case_Statement_Access is access all Case_Statement'Class
     with Storage_Size => 0;

   not overriding function Case_Token
    (Self : Case_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Selecting_Expression
    (Self : Case_Statement)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Is_Token
    (Self : Case_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function End_Token
    (Self : Case_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Case_Token_2
    (Self : Case_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Case_Statement)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Case_Statements;
