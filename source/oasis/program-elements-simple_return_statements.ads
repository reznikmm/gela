--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Simple_Return_Statements is

   pragma Pure (Program.Elements.Simple_Return_Statements);

   type Simple_Return_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Simple_Return_Statement_Access is
     access all Simple_Return_Statement'Class with Storage_Size => 0;

   not overriding function Expression
    (Self : Simple_Return_Statement)
      return Program.Elements.Expressions.Expression_Access is abstract;

   type Simple_Return_Statement_Text is limited interface;

   type Simple_Return_Statement_Text_Access is
     access all Simple_Return_Statement_Text'Class with Storage_Size => 0;

   not overriding function To_Simple_Return_Statement_Text
    (Self : aliased in out Simple_Return_Statement)
      return Simple_Return_Statement_Text_Access is abstract;

   not overriding function Return_Token
    (Self : Simple_Return_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Simple_Return_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Simple_Return_Statements;
