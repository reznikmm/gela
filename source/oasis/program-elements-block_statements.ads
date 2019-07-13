--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
with Program.Element_Vectors;
with Program.Elements.Exception_Handlers;
with Program.Elements.Identifiers;

package Program.Elements.Block_Statements is

   pragma Pure (Program.Elements.Block_Statements);

   type Block_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Block_Statement_Access is access all Block_Statement'Class
     with Storage_Size => 0;

   not overriding function Statement_Identifier
    (Self : Block_Statement)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
     is abstract;

   not overriding function Declarations
    (Self : Block_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Statements
    (Self : Block_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Exception_Handlers
    (Self : Block_Statement)
      return not null Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access is abstract;

   not overriding function End_Statement_Identifier
    (Self : Block_Statement)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

   type Block_Statement_Text is limited interface;

   type Block_Statement_Text_Access is access all Block_Statement_Text'Class
     with Storage_Size => 0;

   not overriding function To_Block_Statement_Text
    (Self : aliased in out Block_Statement)
      return Block_Statement_Text_Access is abstract;

   not overriding function Colon_Token
    (Self : Block_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Declare_Token
    (Self : Block_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Begin_Token
    (Self : Block_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Exception_Token
    (Self : Block_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function End_Token
    (Self : Block_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Block_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Block_Statements;
