--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Definitions;
with Program.Elements.Formal_Type_Definitions;

package Program.Elements.Formal_Type_Declarations is

   pragma Pure (Program.Elements.Formal_Type_Declarations);

   type Formal_Type_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Formal_Type_Declaration_Access is
     access all Formal_Type_Declaration'Class with Storage_Size => 0;

   not overriding function Type_Token
    (Self : Formal_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Formal_Type_Declaration)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
     is abstract;

   not overriding function Discriminant_Part
    (Self : Formal_Type_Declaration)
      return Program.Elements.Definitions.Definition_Access is abstract;

   not overriding function Is_Token
    (Self : Formal_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function View
    (Self : Formal_Type_Declaration)
      return Program.Elements.Formal_Type_Definitions
          .Formal_Type_Definition_Access is abstract;

   not overriding function With_Token
    (Self : Formal_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Formal_Type_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Formal_Type_Declarations;
