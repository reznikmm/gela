--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Formal_Access_Types;
with Program.Lexical_Elements;
with Program.Elements.Subtype_Indications;

package Program.Elements.Formal_Object_Access_Types is

   pragma Pure (Program.Elements.Formal_Object_Access_Types);

   type Formal_Object_Access_Type is
     limited interface
       and Program.Elements.Formal_Access_Types.Formal_Access_Type;

   type Formal_Object_Access_Type_Access is
     access all Formal_Object_Access_Type'Class with Storage_Size => 0;

   not overriding function Subtype_Indication
    (Self : Formal_Object_Access_Type)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access is abstract;

   not overriding function Has_Not_Null
    (Self : Formal_Object_Access_Type)
      return Boolean is abstract;

   not overriding function Has_All
    (Self : Formal_Object_Access_Type)
      return Boolean is abstract;

   not overriding function Has_Constant
    (Self : Formal_Object_Access_Type)
      return Boolean is abstract;

   type Formal_Object_Access_Type_Text is limited interface;

   type Formal_Object_Access_Type_Text_Access is
     access all Formal_Object_Access_Type_Text'Class with Storage_Size => 0;

   not overriding function To_Formal_Object_Access_Type_Text
    (Self : aliased in out Formal_Object_Access_Type)
      return Formal_Object_Access_Type_Text_Access is abstract;

   not overriding function Not_Token
    (Self : Formal_Object_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Null_Token
    (Self : Formal_Object_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Access_Token
    (Self : Formal_Object_Access_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function All_Token
    (Self : Formal_Object_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Constant_Token
    (Self : Formal_Object_Access_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Formal_Object_Access_Types;
