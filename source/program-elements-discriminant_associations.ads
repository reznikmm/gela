--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Associations;
with Program.Tokens;
with Program.Elements.Expressions;
with Program.Element_Vectors;

package Program.Elements.Discriminant_Associations is

   pragma Pure (Program.Elements.Discriminant_Associations);

   type Discriminant_Association is
     limited interface and Program.Elements.Associations.Association;

   type Discriminant_Association_Access is
     access all Discriminant_Association'Class with Storage_Size => 0;

   not overriding function Arrow_Token
    (Self : Discriminant_Association)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Expression
    (Self : Discriminant_Association)
      return Program.Elements.Expressions.Expression_Access is abstract;

   type Discriminant_Association_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Discriminant_Association_Vector_Access is
     access all Discriminant_Association_Vector'Class with Storage_Size => 0;

   overriding function Element
    (Self  : Discriminant_Association_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Discriminant_Association;

   function To_Discriminant_Association
    (Self  : Discriminant_Association_Vector'Class;
     Index : Positive)
      return not null Discriminant_Association_Access
     is (Self.Element (Index).To_Discriminant_Association);

end Program.Elements.Discriminant_Associations;
