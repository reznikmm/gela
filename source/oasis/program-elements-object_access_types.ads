--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Access_Types;
with Program.Lexical_Elements;
with Program.Elements.Subtype_Indications;

package Program.Elements.Object_Access_Types is

   pragma Pure (Program.Elements.Object_Access_Types);

   type Object_Access_Type is
     limited interface and Program.Elements.Access_Types.Access_Type;

   type Object_Access_Type_Access is access all Object_Access_Type'Class
     with Storage_Size => 0;

   not overriding function Subtype_Indication
    (Self : Object_Access_Type)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access is abstract;

   not overriding function Has_Not_Null
    (Self : Object_Access_Type)
      return Boolean is abstract;

   not overriding function Has_All (Self : Object_Access_Type) return Boolean
     is abstract;

   not overriding function Has_Constant
    (Self : Object_Access_Type)
      return Boolean is abstract;

   type Object_Access_Type_Text is limited interface;

   type Object_Access_Type_Text_Access is
     access all Object_Access_Type_Text'Class with Storage_Size => 0;

   not overriding function To_Object_Access_Type_Text
    (Self : aliased in out Object_Access_Type)
      return Object_Access_Type_Text_Access is abstract;

   not overriding function Not_Token
    (Self : Object_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token
    (Self : Object_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Access_Token
    (Self : Object_Access_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function All_Token
    (Self : Object_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Constant_Token
    (Self : Object_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Object_Access_Types;
