--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Tokens;
with Program.Elements.Identifiers;

package Program.Elements.Pragmas is

   pragma Pure (Program.Elements.Pragmas);

   type Pragma_Element is limited interface and Program.Elements.Element;

   type Pragma_Access is access all Pragma_Element'Class
     with Storage_Size => 0;

   not overriding function Pragma_Token
    (Self : Pragma_Element)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Pragma_Element)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Pragma_Element)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Pragma_Element)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Pragma_Element)
      return Program.Tokens.Token_Access is abstract;

end Program.Elements.Pragmas;
