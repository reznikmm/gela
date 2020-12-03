--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Element_Vectors;
with Program.Elements.Identifiers;

package Program.Elements.While_Loop_Statements is

   pragma Pure (Program.Elements.While_Loop_Statements);

   type While_Loop_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type While_Loop_Statement_Access is access all While_Loop_Statement'Class
     with Storage_Size => 0;

   not overriding function Statement_Identifier
    (Self : While_Loop_Statement)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
     is abstract;

   not overriding function Condition
    (Self : While_Loop_Statement)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Statements
    (Self : While_Loop_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function End_Statement_Identifier
    (Self : While_Loop_Statement)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

   type While_Loop_Statement_Text is limited interface;

   type While_Loop_Statement_Text_Access is
     access all While_Loop_Statement_Text'Class with Storage_Size => 0;

   not overriding function To_While_Loop_Statement_Text
    (Self : aliased in out While_Loop_Statement)
      return While_Loop_Statement_Text_Access is abstract;

   not overriding function Colon_Token
    (Self : While_Loop_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function While_Token
    (Self : While_Loop_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Loop_Token
    (Self : While_Loop_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function End_Token
    (Self : While_Loop_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Loop_Token_2
    (Self : While_Loop_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : While_Loop_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.While_Loop_Statements;
