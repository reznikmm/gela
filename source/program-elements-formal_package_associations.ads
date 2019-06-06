--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Associations;
with Program.Elements.Expressions;
with Program.Tokens;
with Program.Element_Vectors;

package Program.Elements.Formal_Package_Associations is

   pragma Pure (Program.Elements.Formal_Package_Associations);

   type Formal_Package_Association is
     limited interface and Program.Elements.Associations.Association;

   type Formal_Package_Association_Access is
     access all Formal_Package_Association'Class with Storage_Size => 0;

   not overriding function Formal_Parameter
    (Self : Formal_Package_Association)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Arrow_Token
    (Self : Formal_Package_Association)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Actual_Parameter
    (Self : Formal_Package_Association)
      return Program.Elements.Expressions.Expression_Access is abstract;

   not overriding function Box_Token
    (Self : Formal_Package_Association)
      return Program.Tokens.Token_Access is abstract;

   type Formal_Package_Association_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Formal_Package_Association_Vector_Access is
     access all Formal_Package_Association_Vector'Class with Storage_Size => 0;

   overriding function Element
    (Self  : Formal_Package_Association_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Formal_Package_Association;

   function To_Formal_Package_Association
    (Self  : Formal_Package_Association_Vector'Class;
     Index : Positive)
      return not null Formal_Package_Association_Access
     is (Self.Element (Index).To_Formal_Package_Association);

end Program.Elements.Formal_Package_Associations;
