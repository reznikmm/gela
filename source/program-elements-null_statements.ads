--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Tokens;

package Program.Elements.Null_Statements is

   pragma Pure (Program.Elements.Null_Statements);

   type Null_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Null_Statement_Access is access all Null_Statement'Class
     with Storage_Size => 0;

   not overriding function Null_Token
    (Self : Null_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Null_Statement)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Null_Statements;
