--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Elements.Associations;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Array_Component_Associations is

   pragma Pure (Program.Elements.Array_Component_Associations);

   type Array_Component_Association is
     limited interface and Program.Elements.Associations.Association;

   type Array_Component_Association_Access is
     access all Array_Component_Association'Class with Storage_Size => 0;

   not overriding function Choices
    (Self : Array_Component_Association)
      return not null Program.Element_Vectors.Element_Vector_Access
     is abstract;

   not overriding function Expression
    (Self : Array_Component_Association)
      return Program.Elements.Expressions.Expression_Access is abstract;

   type Array_Component_Association_Text is limited interface;

   type Array_Component_Association_Text_Access is
     access all Array_Component_Association_Text'Class with Storage_Size => 0;

   not overriding function To_Array_Component_Association_Text
    (Self : aliased Array_Component_Association)
      return Array_Component_Association_Text_Access is abstract;

   not overriding function Arrow_Token
    (Self : Array_Component_Association_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Box_Token
    (Self : Array_Component_Association_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   type Array_Component_Association_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Array_Component_Association_Vector_Access is
     access all Array_Component_Association_Vector'Class
     with Storage_Size => 0;

   overriding function Element
    (Self  : Array_Component_Association_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Array_Component_Association;

   function To_Array_Component_Association
    (Self  : Array_Component_Association_Vector'Class;
     Index : Positive)
      return not null Array_Component_Association_Access
     is (Self.Element (Index).To_Array_Component_Association);

end Program.Elements.Array_Component_Associations;
