--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Tokens;

package Program.Elements.Select_Statements is

   pragma Pure (Program.Elements.Select_Statements);

   type Select_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Select_Statement_Access is access all Select_Statement'Class
     with Storage_Size => 0;

   not overriding function Select_Token
    (Self : Select_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function End_Token
    (Self : Select_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Select_Token_2
    (Self : Select_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Select_Statement)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Select_Statements;
