--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Case_Paths;

package Program.Elements.Case_Statements is

   pragma Pure (Program.Elements.Case_Statements);

   type Case_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Case_Statement_Access is access all Case_Statement'Class
     with Storage_Size => 0;

   not overriding function Selecting_Expression
    (Self : Case_Statement)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Paths
    (Self : Case_Statement)
      return not null Program.Elements.Case_Paths.Case_Path_Vector_Access
     is abstract;

   type Case_Statement_Text is limited interface;

   type Case_Statement_Text_Access is access all Case_Statement_Text'Class
     with Storage_Size => 0;

   not overriding function To_Case_Statement_Text
    (Self : aliased Case_Statement)
      return Case_Statement_Text_Access is abstract;

   not overriding function Case_Token
    (Self : Case_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Is_Token
    (Self : Case_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function End_Token
    (Self : Case_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Case_Token_2
    (Self : Case_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Case_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Case_Statements;
