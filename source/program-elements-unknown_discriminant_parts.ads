--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Tokens;

package Program.Elements.Unknown_Discriminant_Parts is

   pragma Pure (Program.Elements.Unknown_Discriminant_Parts);

   type Unknown_Discriminant_Part is
     limited interface and Program.Elements.Definitions.Definition;

   type Unknown_Discriminant_Part_Access is
     access all Unknown_Discriminant_Part'Class with Storage_Size => 0;

   not overriding function Left_Bracket_Token
    (Self : Unknown_Discriminant_Part)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Box_Token
    (Self : Unknown_Discriminant_Part)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Unknown_Discriminant_Part)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Unknown_Discriminant_Parts;
