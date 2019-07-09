--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Access_Types;
with Program.Elements.Formal_Access_Types;
with Program.Elements.Anonymous_Access_Definitions;
with Program.Lexical_Elements;
with Program.Elements.Subtype_Indications;

package Program.Elements.Object_Access_Types is

   pragma Pure (Program.Elements.Object_Access_Types);

   type Object_Access_Type is
     limited interface and Program.Elements.Access_Types.Access_Type
       and Program.Elements.Formal_Access_Types.Formal_Access_Type
       and Program.Elements.Anonymous_Access_Definitions
         .Anonymous_Access_Definition;

   type Object_Access_Type_Access is access all Object_Access_Type'Class
     with Storage_Size => 0;

   not overriding function Subtype_Indication
    (Self : Object_Access_Type)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access is abstract;

   type Object_Access_Type_Text is limited interface;

   type Object_Access_Type_Text_Access is
     access all Object_Access_Type_Text'Class with Storage_Size => 0;

   not overriding function To_Object_Access_Type_Text
    (Self : aliased Object_Access_Type)
      return Object_Access_Type_Text_Access is abstract;

   not overriding function Not_Token
    (Self : Object_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token
    (Self : Object_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function All_Token
    (Self : Object_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Constant_Token
    (Self : Object_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Object_Access_Types;
