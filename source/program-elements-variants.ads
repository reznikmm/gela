--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Tokens;

package Program.Elements.Variants is

   pragma Pure (Program.Elements.Variants);

   type Variant is
     limited interface and Program.Elements.Definitions.Definition;

   type Variant_Access is access all Variant'Class with Storage_Size => 0;

   not overriding function When_Token
    (Self : Variant)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Arrow_Token
    (Self : Variant)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Variants;
