--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Elements.Qualified_Expressions;
with Program.Tokens;

package Program.Elements.Code_Statements is

   pragma Pure (Program.Elements.Code_Statements);

   type Code_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Code_Statement_Access is access all Code_Statement'Class
     with Storage_Size => 0;

   not overriding function Expression
    (Self : Code_Statement)
      return Program.Elements.Qualified_Expressions.Qualified_Expression_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Code_Statement)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Code_Statements;
