--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Aspect_Specifications;
with Program.Element_Vectors;
with Program.Elements.Exception_Handlers;
with Program.Elements.Identifiers;

package Program.Elements.Task_Body_Declarations is

   pragma Pure (Program.Elements.Task_Body_Declarations);

   type Task_Body_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Task_Body_Declaration_Access is access all Task_Body_Declaration'Class
     with Storage_Size => 0;

   not overriding function Task_Token
    (Self : Task_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Body_Token
    (Self : Task_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Task_Body_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function With_Token
    (Self : Task_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Aspects
    (Self : Task_Body_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is abstract;

   not overriding function Is_Token
    (Self : Task_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Declarations
    (Self : Task_Body_Declaration)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Begin_Token
    (Self : Task_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Statements
    (Self : Task_Body_Declaration)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Exception_Token
    (Self : Task_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Exception_Handlers
    (Self : Task_Body_Declaration)
      return not null Program.Elements.Exception_Handlers
          .Exception_Handler_Vector_Access is abstract;

   not overriding function End_Token
    (Self : Task_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function End_Name
    (Self : Task_Body_Declaration)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Task_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Task_Body_Declarations;
