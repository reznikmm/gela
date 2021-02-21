--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Defining_Names;
with Program.Lexical_Elements;

package Program.Elements.Defining_Character_Literals is

   pragma Pure (Program.Elements.Defining_Character_Literals);

   type Defining_Character_Literal is
     limited interface and Program.Elements.Defining_Names.Defining_Name;

   type Defining_Character_Literal_Access is
     access all Defining_Character_Literal'Class with Storage_Size => 0;

   type Defining_Character_Literal_Text is limited interface;

   type Defining_Character_Literal_Text_Access is
     access all Defining_Character_Literal_Text'Class with Storage_Size => 0;

   not overriding function To_Defining_Character_Literal_Text
    (Self : in out Defining_Character_Literal)
      return Defining_Character_Literal_Text_Access is abstract;

   not overriding function Character_Literal_Token
    (Self : Defining_Character_Literal_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Defining_Character_Literals;
