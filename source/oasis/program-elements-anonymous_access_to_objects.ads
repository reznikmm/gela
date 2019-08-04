--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Anonymous_Access_Definitions;
with Program.Lexical_Elements;
with Program.Elements.Subtype_Indications;

package Program.Elements.Anonymous_Access_To_Objects is

   pragma Pure (Program.Elements.Anonymous_Access_To_Objects);

   type Anonymous_Access_To_Object is
     limited interface
       and Program.Elements.Anonymous_Access_Definitions
         .Anonymous_Access_Definition;

   type Anonymous_Access_To_Object_Access is
     access all Anonymous_Access_To_Object'Class with Storage_Size => 0;

   not overriding function Subtype_Indication
    (Self : Anonymous_Access_To_Object)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access is abstract;

   not overriding function Has_Not_Null
    (Self : Anonymous_Access_To_Object)
      return Boolean is abstract;

   not overriding function Has_All
    (Self : Anonymous_Access_To_Object)
      return Boolean is abstract;

   not overriding function Has_Constant
    (Self : Anonymous_Access_To_Object)
      return Boolean is abstract;

   type Anonymous_Access_To_Object_Text is limited interface;

   type Anonymous_Access_To_Object_Text_Access is
     access all Anonymous_Access_To_Object_Text'Class with Storage_Size => 0;

   not overriding function To_Anonymous_Access_To_Object_Text
    (Self : aliased in out Anonymous_Access_To_Object)
      return Anonymous_Access_To_Object_Text_Access is abstract;

   not overriding function Not_Token
    (Self : Anonymous_Access_To_Object_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token
    (Self : Anonymous_Access_To_Object_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Access_Token
    (Self : Anonymous_Access_To_Object_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function All_Token
    (Self : Anonymous_Access_To_Object_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Constant_Token
    (Self : Anonymous_Access_To_Object_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Anonymous_Access_To_Objects;
