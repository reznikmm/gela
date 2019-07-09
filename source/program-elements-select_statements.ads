--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Lexical_Elements;
with Program.Elements.Select_Paths;
with Program.Element_Vectors;

package Program.Elements.Select_Statements is

   pragma Pure (Program.Elements.Select_Statements);

   type Select_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Select_Statement_Access is access all Select_Statement'Class
     with Storage_Size => 0;

   not overriding function Paths
    (Self : Select_Statement)
      return not null Program.Elements.Select_Paths.Select_Path_Vector_Access
     is abstract;

   not overriding function Then_Abort_Statements
    (Self : Select_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Else_Statements
    (Self : Select_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   type Select_Statement_Text is limited interface;

   type Select_Statement_Text_Access is access all Select_Statement_Text'Class
     with Storage_Size => 0;

   not overriding function To_Select_Statement_Text
    (Self : aliased Select_Statement)
      return Select_Statement_Text_Access is abstract;

   not overriding function Select_Token
    (Self : Select_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Then_Token
    (Self : Select_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Abort_Token
    (Self : Select_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Else_Token
    (Self : Select_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function End_Token
    (Self : Select_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Select_Token_2
    (Self : Select_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Select_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Select_Statements;
