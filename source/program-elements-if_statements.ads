--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.If_Statements is

   pragma Pure (Program.Elements.If_Statements);

   type If_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type If_Statement_Access is access all If_Statement'Class
     with Storage_Size => 0;

   not overriding function If_Token
    (Self : If_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Condition
    (Self : If_Statement)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Then_Token
    (Self : If_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Else_Token
    (Self : If_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function End_Token
    (Self : If_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function If_Token_2
    (Self : If_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : If_Statement)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.If_Statements;
