--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Elements.Type_Definitions;
with Program.Lexical_Elements;
with Program.Elements.Subtype_Indications;

package Program.Elements.Derived_Types is

   pragma Pure (Program.Elements.Derived_Types);

   type Derived_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Derived_Type_Access is access all Derived_Type'Class
     with Storage_Size => 0;

   not overriding function Parent
    (Self : Derived_Type)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access is abstract;

   not overriding function Has_Abstract (Self : Derived_Type) return Boolean
     is abstract;

   not overriding function Has_Limited (Self : Derived_Type) return Boolean
     is abstract;

   type Derived_Type_Text is limited interface;

   type Derived_Type_Text_Access is access all Derived_Type_Text'Class
     with Storage_Size => 0;

   not overriding function To_Derived_Type_Text
    (Self : in out Derived_Type)
      return Derived_Type_Text_Access is abstract;

   not overriding function Abstract_Token
    (Self : Derived_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Limited_Token
    (Self : Derived_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function New_Token
    (Self : Derived_Type_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Derived_Types;
