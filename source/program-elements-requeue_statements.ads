--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.Requeue_Statements is

   pragma Pure (Program.Elements.Requeue_Statements);

   type Requeue_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Requeue_Statement_Access is access all Requeue_Statement'Class
     with Storage_Size => 0;

   not overriding function Requeue_Token
    (Self : Requeue_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Entry_Name
    (Self : Requeue_Statement)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function With_Token
    (Self : Requeue_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Abort_Token
    (Self : Requeue_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Requeue_Statement)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Requeue_Statements;
