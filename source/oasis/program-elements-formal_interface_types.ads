--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Formal_Type_Definitions;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Formal_Interface_Types is

   pragma Pure (Program.Elements.Formal_Interface_Types);

   type Formal_Interface_Type is
     limited interface
       and Program.Elements.Formal_Type_Definitions.Formal_Type_Definition;

   type Formal_Interface_Type_Access is access all Formal_Interface_Type'Class
     with Storage_Size => 0;

   not overriding function Progenitors
    (Self : Formal_Interface_Type)
      return not null Program.Elements.Expressions.Expression_Vector_Access
     is abstract;

   not overriding function Has_Limited
    (Self : Formal_Interface_Type)
      return Boolean is abstract;

   not overriding function Has_Task
    (Self : Formal_Interface_Type)
      return Boolean is abstract;

   not overriding function Has_Protected
    (Self : Formal_Interface_Type)
      return Boolean is abstract;

   not overriding function Has_Synchronized
    (Self : Formal_Interface_Type)
      return Boolean is abstract;

   type Formal_Interface_Type_Text is limited interface;

   type Formal_Interface_Type_Text_Access is
     access all Formal_Interface_Type_Text'Class with Storage_Size => 0;

   not overriding function To_Formal_Interface_Type_Text
    (Self : aliased in out Formal_Interface_Type)
      return Formal_Interface_Type_Text_Access is abstract;

   not overriding function Limited_Token
    (Self : Formal_Interface_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Task_Token
    (Self : Formal_Interface_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Protected_Token
    (Self : Formal_Interface_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Synchronized_Token
    (Self : Formal_Interface_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Interface_Token
    (Self : Formal_Interface_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function And_Token
    (Self : Formal_Interface_Type_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Formal_Interface_Types;
