--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Expressions;

package Program.Elements.Formal_Package_Declarations is

   pragma Pure (Program.Elements.Formal_Package_Declarations);

   type Formal_Package_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Formal_Package_Declaration_Access is
     access all Formal_Package_Declaration'Class with Storage_Size => 0;

   not overriding function With_Token
    (Self : Formal_Package_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Package_Token
    (Self : Formal_Package_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Formal_Package_Declaration)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
     is abstract;

   not overriding function Is_Token
    (Self : Formal_Package_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function New_Token
    (Self : Formal_Package_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Generic_Package_Name
    (Self : Formal_Package_Declaration)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Formal_Package_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Formal_Package_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function With_Token_2
    (Self : Formal_Package_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Formal_Package_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Formal_Package_Declarations;
