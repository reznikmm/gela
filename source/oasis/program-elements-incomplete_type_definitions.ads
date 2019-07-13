--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Lexical_Elements;

package Program.Elements.Incomplete_Type_Definitions is

   pragma Pure (Program.Elements.Incomplete_Type_Definitions);

   type Incomplete_Type_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Incomplete_Type_Definition_Access is
     access all Incomplete_Type_Definition'Class with Storage_Size => 0;

   not overriding function Has_Tagged
    (Self : Incomplete_Type_Definition)
      return Boolean is abstract;

   type Incomplete_Type_Definition_Text is limited interface;

   type Incomplete_Type_Definition_Text_Access is
     access all Incomplete_Type_Definition_Text'Class with Storage_Size => 0;

   not overriding function To_Incomplete_Type_Definition_Text
    (Self : aliased in out Incomplete_Type_Definition)
      return Incomplete_Type_Definition_Text_Access is abstract;

   not overriding function Tagged_Token
    (Self : Incomplete_Type_Definition_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Incomplete_Type_Definitions;
