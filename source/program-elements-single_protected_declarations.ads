--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Protected_Definitions;

package Program.Elements.Single_Protected_Declarations is

   pragma Pure (Program.Elements.Single_Protected_Declarations);

   type Single_Protected_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Single_Protected_Declaration_Access is
     access all Single_Protected_Declaration'Class with Storage_Size => 0;

   not overriding function Protected_Token
    (Self : Single_Protected_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Single_Protected_Declaration)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
     is abstract;

   not overriding function With_Token
    (Self : Single_Protected_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Is_Token
    (Self : Single_Protected_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function New_Token
    (Self : Single_Protected_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function With_Token_2
    (Self : Single_Protected_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Object_Subtype
    (Self : Single_Protected_Declaration)
      return Program.Elements.Protected_Definitions.Protected_Definition_Access
     is abstract;

   not overriding function Semicolon_Token
    (Self : Single_Protected_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Single_Protected_Declarations;
