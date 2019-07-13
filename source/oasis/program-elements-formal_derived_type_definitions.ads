--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Formal_Type_Definitions;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Formal_Derived_Type_Definitions is

   pragma Pure (Program.Elements.Formal_Derived_Type_Definitions);

   type Formal_Derived_Type_Definition is
     limited interface
       and Program.Elements.Formal_Type_Definitions.Formal_Type_Definition;

   type Formal_Derived_Type_Definition_Access is
     access all Formal_Derived_Type_Definition'Class with Storage_Size => 0;

   not overriding function Subtype_Mark
    (Self : Formal_Derived_Type_Definition)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   not overriding function Progenitors
    (Self : Formal_Derived_Type_Definition)
      return not null Program.Elements.Expressions.Expression_Vector_Access
     is abstract;

   not overriding function Has_Abstract
    (Self : Formal_Derived_Type_Definition)
      return Boolean is abstract;

   not overriding function Has_Limited
    (Self : Formal_Derived_Type_Definition)
      return Boolean is abstract;

   not overriding function Has_Synchronized
    (Self : Formal_Derived_Type_Definition)
      return Boolean is abstract;

   not overriding function Has_With_Private
    (Self : Formal_Derived_Type_Definition)
      return Boolean is abstract;

   type Formal_Derived_Type_Definition_Text is limited interface;

   type Formal_Derived_Type_Definition_Text_Access is
     access all Formal_Derived_Type_Definition_Text'Class
     with Storage_Size => 0;

   not overriding function To_Formal_Derived_Type_Definition_Text
    (Self : aliased Formal_Derived_Type_Definition)
      return Formal_Derived_Type_Definition_Text_Access is abstract;

   not overriding function Abstract_Token
    (Self : Formal_Derived_Type_Definition_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Limited_Token
    (Self : Formal_Derived_Type_Definition_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Synchronized_Token
    (Self : Formal_Derived_Type_Definition_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function New_Token
    (Self : Formal_Derived_Type_Definition_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function And_Token
    (Self : Formal_Derived_Type_Definition_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function With_Token
    (Self : Formal_Derived_Type_Definition_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Private_Token
    (Self : Formal_Derived_Type_Definition_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Formal_Derived_Type_Definitions;
