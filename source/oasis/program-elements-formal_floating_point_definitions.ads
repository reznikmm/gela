--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Formal_Type_Definitions;
with Program.Lexical_Elements;

package Program.Elements.Formal_Floating_Point_Definitions is

   pragma Pure (Program.Elements.Formal_Floating_Point_Definitions);

   type Formal_Floating_Point_Definition is
     limited interface
       and Program.Elements.Formal_Type_Definitions.Formal_Type_Definition;

   type Formal_Floating_Point_Definition_Access is
     access all Formal_Floating_Point_Definition'Class with Storage_Size => 0;

   type Formal_Floating_Point_Definition_Text is limited interface;

   type Formal_Floating_Point_Definition_Text_Access is
     access all Formal_Floating_Point_Definition_Text'Class
     with Storage_Size => 0;

   not overriding function To_Formal_Floating_Point_Definition_Text
    (Self : in out Formal_Floating_Point_Definition)
      return Formal_Floating_Point_Definition_Text_Access is abstract;

   not overriding function Digits_Token
    (Self : Formal_Floating_Point_Definition_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Box_Token
    (Self : Formal_Floating_Point_Definition_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Formal_Floating_Point_Definitions;
