--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vectors;

package Program.Elements.Expressions is

   pragma Pure (Program.Elements.Expressions);

   type Expression is limited interface and Program.Elements.Element;

   type Expression_Access is access all Expression'Class
     with Storage_Size => 0;

   type Expression_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Expression_Vector_Access is access all Expression_Vector'Class
     with Storage_Size => 0;

   overriding function Element
    (Self  : Expression_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Expression;

   function To_Expression
    (Self  : Expression_Vector'Class;
     Index : Positive)
      return not null Expression_Access
     is (Self.Element (Index).To_Expression);

end Program.Elements.Expressions;
