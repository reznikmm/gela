--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Formal_Type_Definitions;
with Program.Lexical_Elements;

package Program.Elements.Formal_Discrete_Type_Definitions is

   pragma Pure (Program.Elements.Formal_Discrete_Type_Definitions);

   type Formal_Discrete_Type_Definition is
     limited interface
       and Program.Elements.Formal_Type_Definitions.Formal_Type_Definition;

   type Formal_Discrete_Type_Definition_Access is
     access all Formal_Discrete_Type_Definition'Class with Storage_Size => 0;

   type Formal_Discrete_Type_Definition_Text is limited interface;

   type Formal_Discrete_Type_Definition_Text_Access is
     access all Formal_Discrete_Type_Definition_Text'Class
     with Storage_Size => 0;

   not overriding function To_Formal_Discrete_Type_Definition_Text
    (Self : in out Formal_Discrete_Type_Definition)
      return Formal_Discrete_Type_Definition_Text_Access is abstract;

   not overriding function Left_Bracket_Token
    (Self : Formal_Discrete_Type_Definition_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Box_Token
    (Self : Formal_Discrete_Type_Definition_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Right_Bracket_Token
    (Self : Formal_Discrete_Type_Definition_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Formal_Discrete_Type_Definitions;
