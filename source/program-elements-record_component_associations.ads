--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Associations;
with Program.Tokens;
with Program.Elements.Expressions;
with Program.Element_Vectors;

package Program.Elements.Record_Component_Associations is

   pragma Pure (Program.Elements.Record_Component_Associations);

   type Record_Component_Association is
     limited interface and Program.Elements.Associations.Association;

   type Record_Component_Association_Access is
     access all Record_Component_Association'Class with Storage_Size => 0;

   not overriding function Arrow_Token
    (Self : Record_Component_Association)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Expression
    (Self : Record_Component_Association)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Box_Token
    (Self : Record_Component_Association)
      return Program.Tokens.Token_Access is abstract;

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
