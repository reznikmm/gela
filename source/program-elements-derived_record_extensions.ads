--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;
with Program.Lexical_Elements;
with Program.Elements.Expressions;
with Program.Elements.Definitions;

package Program.Elements.Derived_Record_Extensions is

   pragma Pure (Program.Elements.Derived_Record_Extensions);

   type Derived_Record_Extension is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Derived_Record_Extension_Access is
     access all Derived_Record_Extension'Class with Storage_Size => 0;

   not overriding function Parent
    (Self : Derived_Record_Extension)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Progenitors
    (Self : Derived_Record_Extension)
      return not null Program.Elements.Expressions.Expression_Vector_Access
     is abstract;

   not overriding function Record_Definition
    (Self : Derived_Record_Extension)
      return not null Program.Elements.Definitions.Definition_Access
     is abstract;

   not overriding function Has_Abstract
    (Self : Derived_Record_Extension)
      return Boolean is abstract;

   not overriding function Has_Limited
    (Self : Derived_Record_Extension)
      return Boolean is abstract;

   type Derived_Record_Extension_Text is limited interface;

   type Derived_Record_Extension_Text_Access is
     access all Derived_Record_Extension_Text'Class with Storage_Size => 0;

   not overriding function To_Derived_Record_Extension_Text
    (Self : aliased Derived_Record_Extension)
      return Derived_Record_Extension_Text_Access is abstract;

   not overriding function Abstract_Token
    (Self : Derived_Record_Extension_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Limited_Token
    (Self : Derived_Record_Extension_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function New_Token
    (Self : Derived_Record_Extension_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function And_Token
    (Self : Derived_Record_Extension_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function With_Token
    (Self : Derived_Record_Extension_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

end Program.Elements.Derived_Record_Extensions;
