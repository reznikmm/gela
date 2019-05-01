--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Tokens;
with Program.Elements.Return_Object_Specifications;

package Program.Elements.Extended_Return_Statements is

   pragma Pure (Program.Elements.Extended_Return_Statements);

   type Extended_Return_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Extended_Return_Statement_Access is
     access all Extended_Return_Statement'Class with Storage_Size => 0;

   not overriding function Return_Token
    (Self : Extended_Return_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Return_Object_Specification
    (Self : Extended_Return_Statement)
      return Program.Elements.Return_Object_Specifications
          .Return_Object_Specification_Access is abstract;

   not overriding function Do_Token
    (Self : Extended_Return_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Exception_Token
    (Self : Extended_Return_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function End_Token
    (Self : Extended_Return_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Return_Token_2
    (Self : Extended_Return_Statement)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Extended_Return_Statement)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Extended_Return_Statements;
