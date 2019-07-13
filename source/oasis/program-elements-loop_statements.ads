--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Element_Vectors;
with Program.Elements.Identifiers;

package Program.Elements.Loop_Statements is

   pragma Pure (Program.Elements.Loop_Statements);

   type Loop_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Loop_Statement_Access is access all Loop_Statement'Class
     with Storage_Size => 0;

   not overriding function Statement_Identifier
    (Self : Loop_Statement)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
     is abstract;

   not overriding function Statements
    (Self : Loop_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function End_Statement_Identifier
    (Self : Loop_Statement)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

   type Loop_Statement_Text is limited interface;

   type Loop_Statement_Text_Access is access all Loop_Statement_Text'Class
     with Storage_Size => 0;

   not overriding function To_Loop_Statement_Text
    (Self : aliased in out Loop_Statement)
      return Loop_Statement_Text_Access is abstract;

   not overriding function Colon_Token
    (Self : Loop_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Loop_Token
    (Self : Loop_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function End_Token
    (Self : Loop_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Loop_Token_2
    (Self : Loop_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Loop_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Loop_Statements;
