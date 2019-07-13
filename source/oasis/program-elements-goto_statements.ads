--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Goto_Statements is

   pragma Pure (Program.Elements.Goto_Statements);

   type Goto_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Goto_Statement_Access is access all Goto_Statement'Class
     with Storage_Size => 0;

   not overriding function Goto_Label
    (Self : Goto_Statement)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Goto_Statement_Text is limited interface;

   type Goto_Statement_Text_Access is access all Goto_Statement_Text'Class
     with Storage_Size => 0;

   not overriding function To_Goto_Statement_Text
    (Self : aliased in out Goto_Statement)
      return Goto_Statement_Text_Access is abstract;

   not overriding function Goto_Token
    (Self : Goto_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Goto_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Goto_Statements;
