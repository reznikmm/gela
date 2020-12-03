--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Elements.Associations;
with Program.Elements.Expressions;
with Program.Lexical_Elements;

package Program.Elements.Parameter_Associations is

   pragma Pure (Program.Elements.Parameter_Associations);

   type Parameter_Association is
     limited interface and Program.Elements.Associations.Association;

   type Parameter_Association_Access is access all Parameter_Association'Class
     with Storage_Size => 0;

   not overriding function Formal_Parameter
    (Self : Parameter_Association)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Actual_Parameter
    (Self : Parameter_Association)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Parameter_Association_Text is limited interface;

   type Parameter_Association_Text_Access is
     access all Parameter_Association_Text'Class with Storage_Size => 0;

   not overriding function To_Parameter_Association_Text
    (Self : aliased in out Parameter_Association)
      return Parameter_Association_Text_Access is abstract;

   not overriding function Arrow_Token
    (Self : Parameter_Association_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

   type Parameter_Association_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Parameter_Association_Vector_Access is
     access all Parameter_Association_Vector'Class with Storage_Size => 0;

   overriding function Element
    (Self  : Parameter_Association_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Parameter_Association;

   function To_Parameter_Association
    (Self  : Parameter_Association_Vector'Class;
     Index : Positive)
      return not null Parameter_Association_Access
     is (Self.Element (Index).To_Parameter_Association);

end Program.Elements.Parameter_Associations;
