--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;
with Program.Lexical_Elements;
with Program.Elements.Enumeration_Literal_Specifications;

package Program.Elements.Enumeration_Types is

   pragma Pure (Program.Elements.Enumeration_Types);

   type Enumeration_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Enumeration_Type_Access is access all Enumeration_Type'Class
     with Storage_Size => 0;

   not overriding function Literals
    (Self : Enumeration_Type)
      return not null Program.Elements.Enumeration_Literal_Specifications
          .Enumeration_Literal_Specification_Vector_Access is abstract;

   type Enumeration_Type_Text is limited interface;

   type Enumeration_Type_Text_Access is access all Enumeration_Type_Text'Class
     with Storage_Size => 0;

   not overriding function To_Enumeration_Type_Text
    (Self : aliased in out Enumeration_Type)
      return Enumeration_Type_Text_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Enumeration_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Right_Bracket_Token
    (Self : Enumeration_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Enumeration_Types;
