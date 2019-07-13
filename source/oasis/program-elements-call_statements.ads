--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Parameter_Associations;

package Program.Elements.Call_Statements is

   pragma Pure (Program.Elements.Call_Statements);

   type Call_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Call_Statement_Access is access all Call_Statement'Class
     with Storage_Size => 0;

   not overriding function Called_Name
    (Self : Call_Statement)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Parameters
    (Self : Call_Statement)
      return not null Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access is abstract;

   type Call_Statement_Text is limited interface;

   type Call_Statement_Text_Access is access all Call_Statement_Text'Class
     with Storage_Size => 0;

   not overriding function To_Call_Statement_Text
    (Self : aliased Call_Statement)
      return Call_Statement_Text_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Call_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Call_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Call_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Call_Statements;
