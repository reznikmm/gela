--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Lexical_Elements;
with Program.Elements.Identifiers;
with Program.Elements.Expressions;
with Program.Elements.Parameter_Specifications;
with Program.Element_Vectors;
with Program.Elements.Exception_Handlers;

package Program.Elements.Accept_Statements is

   pragma Pure (Program.Elements.Accept_Statements);

   type Accept_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Accept_Statement_Access is access all Accept_Statement'Class
     with Storage_Size => 0;

   not overriding function Entry_Name
    (Self : Accept_Statement)
      return not null Program.Elements.Identifiers.Identifier_Access
     is abstract;

   not overriding function Entry_Index
    (Self : Accept_Statement)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Parameters
    (Self : Accept_Statement)
      return Program.Elements.Parameter_Specifications
          .Parameter_Specification_Vector_Access is abstract;

   not overriding function Statements
    (Self : Accept_Statement)
      return Program.Element_Vectors.Element_Vector_Access is abstract;

   not overriding function Exception_Handlers
    (Self : Accept_Statement)
      return Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access is abstract;

   not overriding function End_Statement_Identifier
    (Self : Accept_Statement)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

   type Accept_Statement_Text is limited interface;

   type Accept_Statement_Text_Access is access all Accept_Statement_Text'Class
     with Storage_Size => 0;

   not overriding function To_Accept_Statement_Text
    (Self : aliased in out Accept_Statement)
      return Accept_Statement_Text_Access is abstract;

   not overriding function Accept_Token
    (Self : Accept_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Accept_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Accept_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Left_Bracket_Token_2
    (Self : Accept_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Right_Bracket_Token_2
    (Self : Accept_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Do_Token
    (Self : Accept_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Exception_Token
    (Self : Accept_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function End_Token
    (Self : Accept_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Accept_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Accept_Statements;
