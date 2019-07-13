--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Statements;
with Program.Lexical_Elements;
with Program.Elements.Return_Object_Specifications;
with Program.Element_Vectors;
with Program.Elements.Exception_Handlers;

package Program.Elements.Extended_Return_Statements is

   pragma Pure (Program.Elements.Extended_Return_Statements);

   type Extended_Return_Statement is
     limited interface and Program.Elements.Statements.Statement;

   type Extended_Return_Statement_Access is
     access all Extended_Return_Statement'Class with Storage_Size => 0;

   not overriding function Return_Object
    (Self : Extended_Return_Statement)
      return not null Program.Elements.Return_Object_Specifications
          .Return_Object_Specification_Access is abstract;

   not overriding function Statements
    (Self : Extended_Return_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Exception_Handlers
    (Self : Extended_Return_Statement)
      return not null Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access is abstract;

   type Extended_Return_Statement_Text is limited interface;

   type Extended_Return_Statement_Text_Access is
     access all Extended_Return_Statement_Text'Class with Storage_Size => 0;

   not overriding function To_Extended_Return_Statement_Text
    (Self : aliased in out Extended_Return_Statement)
      return Extended_Return_Statement_Text_Access is abstract;

   not overriding function Return_Token
    (Self : Extended_Return_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Do_Token
    (Self : Extended_Return_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Exception_Token
    (Self : Extended_Return_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function End_Token
    (Self : Extended_Return_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Return_Token_2
    (Self : Extended_Return_Statement_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Extended_Return_Statement_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Extended_Return_Statements;
