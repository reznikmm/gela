--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;
with Program.Lexical_Elements;
with Program.Elements.Definitions;

package Program.Elements.Record_Types is

   pragma Pure (Program.Elements.Record_Types);

   type Record_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Record_Type_Access is access all Record_Type'Class
     with Storage_Size => 0;

   not overriding function Record_Definition
    (Self : Record_Type)
      return not null Program.Elements.Definitions.Definition_Access
     is abstract;

   type Record_Type_Text is limited interface;

   type Record_Type_Text_Access is access all Record_Type_Text'Class
     with Storage_Size => 0;

   not overriding function To_Record_Type_Text
    (Self : aliased Record_Type)
      return Record_Type_Text_Access is abstract;

   not overriding function Abstract_Token
    (Self : Record_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Tagged_Token
    (Self : Record_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Limited_Token
    (Self : Record_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Record_Types;
