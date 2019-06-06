--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Associations;
with Program.Elements.Expressions;
with Program.Tokens;
with Program.Element_Vectors;

package Program.Elements.Parameter_Associations is

   pragma Pure (Program.Elements.Parameter_Associations);

   type Parameter_Association is
     limited interface and Program.Elements.Associations.Association;

   type Parameter_Association_Access is access all Parameter_Association'Class
     with Storage_Size => 0;

   not overriding function Formal_Parameter
    (Self : Parameter_Association)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Arrow_Token
    (Self : Parameter_Association)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Actual_Parameter
    (Self : Parameter_Association)
      return Program.Elements.Expressions.Expression_Access is abstract;

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
