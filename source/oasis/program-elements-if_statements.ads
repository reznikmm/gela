--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Element_Vectors;
with Program.Elements.Elsif_Paths;

package Program.Elements.If_Statements is

   pragma Pure (Program.Elements.If_Statements);

   type If_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type If_Statement_Access is access all If_Statement'Class
     with Storage_Size => 0;

   not overriding function Condition
    (Self : If_Statement)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Then_Statements
    (Self : If_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Elsif_Paths
    (Self : If_Statement)
      return not null Program.Elements.Elsif_Paths.Elsif_Path_Vector_Access
     is abstract;

   not overriding function Else_Statements
    (Self : If_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   type If_Statement_Text is limited interface;

   type If_Statement_Text_Access is access all If_Statement_Text'Class
     with Storage_Size => 0;

   not overriding function To_If_Statement_Text
    (Self : aliased If_Statement)
      return If_Statement_Text_Access is abstract;

   not overriding function If_Token
    (Self : If_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Then_Token
    (Self : If_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Else_Token
    (Self : If_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function End_Token
    (Self : If_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function If_Token_2
    (Self : If_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : If_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.If_Statements;
