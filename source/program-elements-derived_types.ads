--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Derived_Types is

   pragma Pure (Program.Elements.Derived_Types);

   type Derived_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Derived_Type_Access is access all Derived_Type'Class
     with Storage_Size => 0;

   not overriding function Parent
    (Self : Derived_Type)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Derived_Type_Text is limited interface;

   type Derived_Type_Text_Access is access all Derived_Type_Text'Class
     with Storage_Size => 0;

   not overriding function To_Derived_Type_Text
    (Self : aliased Derived_Type)
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
