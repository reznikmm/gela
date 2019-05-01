--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Identifiers;

package Program.Elements.Protected_Body_Declarations is

   pragma Pure (Program.Elements.Protected_Body_Declarations);

   type Protected_Body_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Protected_Body_Declaration_Access is
     access all Protected_Body_Declaration'Class with Storage_Size => 0;

   not overriding function Protected_Token
    (Self : Protected_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Body_Token
    (Self : Protected_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Protected_Body_Declaration)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
     is abstract;

   not overriding function With_Token
    (Self : Protected_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Is_Token
    (Self : Protected_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function End_Token
    (Self : Protected_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function End_Name
    (Self : Protected_Body_Declaration)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Protected_Body_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Protected_Body_Declarations;
