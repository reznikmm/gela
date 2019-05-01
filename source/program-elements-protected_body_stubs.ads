--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Identifiers;

package Program.Elements.Protected_Body_Stubs is

   pragma Pure (Program.Elements.Protected_Body_Stubs);

   type Protected_Body_Stub is
     limited interface and Program.Elements.Declarations.Declaration;

   type Protected_Body_Stub_Access is access all Protected_Body_Stub'Class
     with Storage_Size => 0;

   not overriding function Protected_Token
    (Self : Protected_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Body_Token
    (Self : Protected_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Protected_Body_Stub)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
     is abstract;

   not overriding function Is_Token
    (Self : Protected_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Separate_Token
    (Self : Protected_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function With_Token
    (Self : Protected_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Protected_Body_Stub)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Protected_Body_Stubs;
