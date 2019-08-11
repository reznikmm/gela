--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Names;
with Program.Elements.Expressions;
with Program.Lexical_Elements;
with Program.Elements.Defining_Identifiers;

package Program.Elements.Defining_Expanded_Names is

   pragma Pure (Program.Elements.Defining_Expanded_Names);

   type Defining_Expanded_Name is
     limited interface and Program.Elements.Defining_Names.Defining_Name;

   type Defining_Expanded_Name_Access is
     access all Defining_Expanded_Name'Class with Storage_Size => 0;

   not overriding function Prefix
    (Self : Defining_Expanded_Name)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Selector
    (Self : Defining_Expanded_Name)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   type Defining_Expanded_Name_Text is limited interface;

   type Defining_Expanded_Name_Text_Access is
     access all Defining_Expanded_Name_Text'Class with Storage_Size => 0;

   not overriding function To_Defining_Expanded_Name_Text
    (Self : aliased in out Defining_Expanded_Name)
      return Defining_Expanded_Name_Text_Access is abstract;

   not overriding function Dot_Token
    (Self : Defining_Expanded_Name_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Defining_Expanded_Names;
