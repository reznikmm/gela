--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Component_Definitions;
with Program.Elements.Expressions;

package Program.Elements.Component_Declarations is

   pragma Pure (Program.Elements.Component_Declarations);

   type Component_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Component_Declaration_Access is access all Component_Declaration'Class
     with Storage_Size => 0;

   not overriding function Colon_Token
    (Self : Component_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Object_Subtype
    (Self : Component_Declaration)
      return Program.Elements.Component_Definitions.Component_Definition_Access
     is abstract;

   not overriding function Assignment_Token
    (Self : Component_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Default_Expression
    (Self : Component_Declaration)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function With_Token
    (Self : Component_Declaration)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Component_Declaration)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Component_Declarations;
