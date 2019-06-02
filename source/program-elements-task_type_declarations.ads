--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Known_Discriminant_Parts;
with Program.Elements.Task_Definitions;

package Program.Elements.Task_Type_Declarations is

   pragma Pure (Program.Elements.Task_Type_Declarations);

   type Task_Type_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Task_Type_Declaration_Access is access all Task_Type_Declaration'Class
     with Storage_Size => 0;

   not overriding function Task_Token
    (Self : Task_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Type_Token
    (Self : Task_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Task_Type_Declaration)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
     is abstract;

   not overriding function Discriminant_Part
    (Self : Task_Type_Declaration)
      return Program.Elements.Known_Discriminant_Parts
          .Known_Discriminant_Part_Access is abstract;

   not overriding function With_Token
    (Self : Task_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Is_Token
    (Self : Task_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function New_Token
    (Self : Task_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function And_Token
    (Self : Task_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function With_Token_2
    (Self : Task_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Definition
    (Self : Task_Type_Declaration)
      return Program.Elements.Task_Definitions.Task_Definition_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Task_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Task_Type_Declarations;
