--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Elements.Expressions;
with Program.Lexical_Elements;

package Program.Elements.Assignment_Statements is

   pragma Pure (Program.Elements.Assignment_Statements);

   type Assignment_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Assignment_Statement_Access is access all Assignment_Statement'Class
     with Storage_Size => 0;

   not overriding function Variable_Name
    (Self : Assignment_Statement)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Expression
    (Self : Assignment_Statement)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Assignment_Statement_Text is limited interface;

   type Assignment_Statement_Text_Access is
     access all Assignment_Statement_Text'Class with Storage_Size => 0;

   not overriding function To_Assignment_Statement_Text
    (Self : aliased in out Assignment_Statement)
      return Assignment_Statement_Text_Access is abstract;

   not overriding function Assignment_Token
    (Self : Assignment_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Assignment_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Assignment_Statements;
