--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Tokens;
with Program.Elements.Identifiers;

package Program.Elements.Variant_Parts is

   pragma Pure (Program.Elements.Variant_Parts);

   type Variant_Part is
     limited interface and Program.Elements.Definitions.Definition;

   type Variant_Part_Access is access all Variant_Part'Class
     with Storage_Size => 0;

   not overriding function Case_Token
    (Self : Variant_Part)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Discriminant
    (Self : Variant_Part)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

   not overriding function Is_Token
    (Self : Variant_Part)
      return Program.Tokens.Token_Access is abstract;

   not overriding function End_Token
    (Self : Variant_Part)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Case_Token_2
    (Self : Variant_Part)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Variant_Part)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Variant_Parts;
