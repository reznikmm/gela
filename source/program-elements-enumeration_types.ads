--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;
with Program.Tokens;

package Program.Elements.Enumeration_Types is

   pragma Pure (Program.Elements.Enumeration_Types);

   type Enumeration_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Enumeration_Type_Access is access all Enumeration_Type'Class
     with Storage_Size => 0;

   not overriding function Left_Bracket_Token
    (Self : Enumeration_Type)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Enumeration_Type)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Enumeration_Types;
