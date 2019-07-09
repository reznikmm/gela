--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Lexical_Elements;

package Program.Elements.Private_Type_Definitions is

   pragma Pure (Program.Elements.Private_Type_Definitions);

   type Private_Type_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Private_Type_Definition_Access is
     access all Private_Type_Definition'Class with Storage_Size => 0;

   type Private_Type_Definition_Text is limited interface;

   type Private_Type_Definition_Text_Access is
     access all Private_Type_Definition_Text'Class with Storage_Size => 0;

   not overriding function To_Private_Type_Definition_Text
    (Self : aliased Private_Type_Definition)
      return Private_Type_Definition_Text_Access is abstract;

   not overriding function Abstract_Token
    (Self : Private_Type_Definition_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Tagged_Token
    (Self : Private_Type_Definition_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Limited_Token
    (Self : Private_Type_Definition_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Private_Token
    (Self : Private_Type_Definition_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Private_Type_Definitions;
