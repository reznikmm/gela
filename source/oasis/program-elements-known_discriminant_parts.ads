--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Lexical_Elements;
with Program.Elements.Discriminant_Specifications;

package Program.Elements.Known_Discriminant_Parts is

   pragma Pure (Program.Elements.Known_Discriminant_Parts);

   type Known_Discriminant_Part is
     limited interface and Program.Elements.Definitions.Definition;

   type Known_Discriminant_Part_Access is
     access all Known_Discriminant_Part'Class with Storage_Size => 0;

   not overriding function Discriminants
    (Self : Known_Discriminant_Part)
      return not null Program.Elements.Discriminant_Specifications
          .Discriminant_Specification_Vector_Access is abstract;

   type Known_Discriminant_Part_Text is limited interface;

   type Known_Discriminant_Part_Text_Access is
     access all Known_Discriminant_Part_Text'Class with Storage_Size => 0;

   not overriding function To_Known_Discriminant_Part_Text
    (Self : aliased in out Known_Discriminant_Part)
      return Known_Discriminant_Part_Text_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Known_Discriminant_Part_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Right_Bracket_Token
    (Self : Known_Discriminant_Part_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Known_Discriminant_Parts;
