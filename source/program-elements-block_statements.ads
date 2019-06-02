--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Elements.Defining_Identifiers;
with Program.Tokens;
with Program.Elements.Identifiers;

package Program.Elements.Block_Statements is

   pragma Pure (Program.Elements.Block_Statements);

   type Block_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Block_Statement_Access is access all Block_Statement'Class
     with Storage_Size => 0;

   not overriding function Statement_Identifier
    (Self : Block_Statement)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
     is abstract;

   not overriding function Colon_Token
    (Self : Block_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Declare_Token
    (Self : Block_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Begin_Token
    (Self : Block_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Exception_Token
    (Self : Block_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function End_Token
    (Self : Block_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function End_Statement_Identifier
    (Self : Block_Statement)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Block_Statement)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Block_Statements;
