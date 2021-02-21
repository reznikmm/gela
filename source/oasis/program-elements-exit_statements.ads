--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Exit_Statements is

   pragma Pure (Program.Elements.Exit_Statements);

   type Exit_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Exit_Statement_Access is access all Exit_Statement'Class
     with Storage_Size => 0;

   not overriding function Exit_Loop_Name
    (Self : Exit_Statement)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Condition
    (Self : Exit_Statement)
      return Program.Elements.Expressions.Expression_Access is abstract;

   type Exit_Statement_Text is limited interface;

   type Exit_Statement_Text_Access is access all Exit_Statement_Text'Class
     with Storage_Size => 0;

   not overriding function To_Exit_Statement_Text
    (Self : in out Exit_Statement)
      return Exit_Statement_Text_Access is abstract;

   not overriding function Exit_Token
    (Self : Exit_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function When_Token
    (Self : Exit_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Exit_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Exit_Statements;
