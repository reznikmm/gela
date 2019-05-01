--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Elements.Defining_Identifiers;
with Program.Tokens;
with Program.Elements.Expressions;
with Program.Elements.Identifiers;

package Program.Elements.While_Loop_Statements is

   pragma Pure (Program.Elements.While_Loop_Statements);

   type While_Loop_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type While_Loop_Statement_Access is access all While_Loop_Statement'Class
     with Storage_Size => 0;

   not overriding function Statement_Identifier
    (Self : While_Loop_Statement)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
     is abstract;

   not overriding function Colon_Token
    (Self : While_Loop_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function While_Token
    (Self : While_Loop_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function While_Condition
    (Self : While_Loop_Statement)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Loop_Token
    (Self : While_Loop_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function End_Token
    (Self : While_Loop_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Loop_Token_2
    (Self : While_Loop_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Statement_Identifier
    (Self : While_Loop_Statement)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

   not overriding function Semicolon_Token
    (Self : While_Loop_Statement)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.While_Loop_Statements;
