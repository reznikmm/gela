--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Elements.Associations;
with Program.Lexical_Elements;
with Program.Elements.Expressions;

package Program.Elements.Record_Component_Associations is

   pragma Pure (Program.Elements.Record_Component_Associations);

   type Record_Component_Association is
     limited interface and Program.Elements.Associations.Association;

   type Record_Component_Association_Access is
     access all Record_Component_Association'Class with Storage_Size => 0;

   not overriding function Choices
    (Self : Record_Component_Association)
      return Program.Element_Vectors.Element_Vector_Access is abstract;

   not overriding function Expression
    (Self : Record_Component_Association)
      return Program.Elements.Expressions.Expression_Access is abstract;

   type Record_Component_Association_Text is limited interface;

   type Record_Component_Association_Text_Access is
     access all Record_Component_Association_Text'Class with Storage_Size => 0;

   not overriding function To_Record_Component_Association_Text
    (Self : aliased in out Record_Component_Association)
      return Record_Component_Association_Text_Access is abstract;

   not overriding function Arrow_Token
    (Self : Record_Component_Association_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   not overriding function Box_Token
    (Self : Record_Component_Association_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   type Record_Component_Association_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Record_Component_Association_Vector_Access is
     access all Record_Component_Association_Vector'Class
     with Storage_Size => 0;

   overriding function Element
    (Self  : Record_Component_Association_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Record_Component_Association;

   function To_Record_Component_Association
    (Self  : Record_Component_Association_Vector'Class;
     Index : Positive)
      return not null Record_Component_Association_Access
     is (Self.Element (Index).To_Record_Component_Association);

end Program.Elements.Record_Component_Associations;
