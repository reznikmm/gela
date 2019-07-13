--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Elements.Qualified_Expressions;
with Program.Lexical_Elements;

package Program.Elements.Code_Statements is

   pragma Pure (Program.Elements.Code_Statements);

   type Code_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Code_Statement_Access is access all Code_Statement'Class
     with Storage_Size => 0;

   not overriding function Expression
    (Self : Code_Statement)
      return not null Program.Elements.Qualified_Expressions
          .Qualified_Expression_Access is abstract;

   type Code_Statement_Text is limited interface;

   type Code_Statement_Text_Access is access all Code_Statement_Text'Class
     with Storage_Size => 0;

   not overriding function To_Code_Statement_Text
    (Self : aliased Code_Statement)
      return Code_Statement_Text_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Code_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Code_Statements;
