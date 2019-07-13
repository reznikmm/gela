--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Lexical_Elements;
with Program.Element_Vectors;

package Program.Elements.Record_Definitions is

   pragma Pure (Program.Elements.Record_Definitions);

   type Record_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Record_Definition_Access is access all Record_Definition'Class
     with Storage_Size => 0;

   not overriding function Components
    (Self : Record_Definition)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   type Record_Definition_Text is limited interface;

   type Record_Definition_Text_Access is
     access all Record_Definition_Text'Class with Storage_Size => 0;

   not overriding function To_Record_Definition_Text
    (Self : aliased in out Record_Definition)
      return Record_Definition_Text_Access is abstract;

   not overriding function Record_Token
    (Self : Record_Definition_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function End_Token
    (Self : Record_Definition_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Record_Token_2
    (Self : Record_Definition_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Record_Definitions;
