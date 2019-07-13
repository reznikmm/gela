--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;

package Program.Elements.Choice_Parameter_Specifications is

   pragma Pure (Program.Elements.Choice_Parameter_Specifications);

   type Choice_Parameter_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Choice_Parameter_Specification_Access is
     access all Choice_Parameter_Specification'Class with Storage_Size => 0;

   not overriding function Name
    (Self : Choice_Parameter_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   type Choice_Parameter_Specification_Text is limited interface;

   type Choice_Parameter_Specification_Text_Access is
     access all Choice_Parameter_Specification_Text'Class
     with Storage_Size => 0;

   not overriding function To_Choice_Parameter_Specification_Text
    (Self : aliased in out Choice_Parameter_Specification)
      return Choice_Parameter_Specification_Text_Access is abstract;

   not overriding function Colon_Token
    (Self : Choice_Parameter_Specification_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Choice_Parameter_Specifications;
