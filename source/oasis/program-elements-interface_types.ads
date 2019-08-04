--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Interface_Types is

   pragma Pure (Program.Elements.Interface_Types);

   type Interface_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Interface_Type_Access is access all Interface_Type'Class
     with Storage_Size => 0;

   not overriding function Progenitors
    (Self : Interface_Type)
      return not null Program.Elements.Expressions.Expression_Vector_Access
     is abstract;

   not overriding function Has_Limited (Self : Interface_Type) return Boolean
     is abstract;

   not overriding function Has_Task (Self : Interface_Type) return Boolean
     is abstract;

   not overriding function Has_Protected (Self : Interface_Type) return Boolean
     is abstract;

   not overriding function Has_Synchronized
    (Self : Interface_Type)
      return Boolean is abstract;

   type Interface_Type_Text is limited interface;

   type Interface_Type_Text_Access is access all Interface_Type_Text'Class
     with Storage_Size => 0;

   not overriding function To_Interface_Type_Text
    (Self : aliased in out Interface_Type)
      return Interface_Type_Text_Access is abstract;

   not overriding function Limited_Token
    (Self : Interface_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Task_Token
    (Self : Interface_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Protected_Token
    (Self : Interface_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Synchronized_Token
    (Self : Interface_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Interface_Token
    (Self : Interface_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function And_Token
    (Self : Interface_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Interface_Types;
