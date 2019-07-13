--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Element_Vectors;
with Program.Lexical_Elements;
with Program.Elements.Identifiers;

package Program.Elements.Protected_Definitions is

   pragma Pure (Program.Elements.Protected_Definitions);

   type Protected_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Protected_Definition_Access is access all Protected_Definition'Class
     with Storage_Size => 0;

   not overriding function Visible_Declarations
    (Self : Protected_Definition)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Private_Declarations
    (Self : Protected_Definition)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function End_Name
    (Self : Protected_Definition)
      return Program.Elements.Identifiers.Identifier_Access is abstract;

   type Protected_Definition_Text is limited interface;

   type Protected_Definition_Text_Access is
     access all Protected_Definition_Text'Class with Storage_Size => 0;

   not overriding function To_Protected_Definition_Text
    (Self : aliased Protected_Definition)
      return Protected_Definition_Text_Access is abstract;

   not overriding function Private_Token
    (Self : Protected_Definition_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function End_Token
    (Self : Protected_Definition_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Protected_Definitions;
