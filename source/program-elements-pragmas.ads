--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Identifiers;
with Program.Elements.Parameter_Associations;

package Program.Elements.Pragmas is

   pragma Pure (Program.Elements.Pragmas);

   type Pragma_Element is limited interface and Program.Elements.Element;

   type Pragma_Access is access all Pragma_Element'Class
     with Storage_Size => 0;

   not overriding function Name
    (Self : Pragma_Element)
      return not null Program.Elements.Identifiers.Identifier_Access
     is abstract;

   not overriding function Arguments
    (Self : Pragma_Element)
      return not null Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access is abstract;

   type Pragma_Text is limited interface;

   type Pragma_Text_Access is access all Pragma_Text'Class
     with Storage_Size => 0;

   not overriding function To_Pragma_Text
    (Self : aliased Pragma_Element)
      return Pragma_Text_Access is abstract;

   not overriding function Pragma_Token
    (Self : Pragma_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Left_Bracket_Token
    (Self : Pragma_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Right_Bracket_Token
    (Self : Pragma_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Semicolon_Token
    (Self : Pragma_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Pragmas;
