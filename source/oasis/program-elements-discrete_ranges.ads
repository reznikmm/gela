--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Element_Vectors;
with Program.Elements.Definitions;

package Program.Elements.Discrete_Ranges is

   pragma Pure (Program.Elements.Discrete_Ranges);

   type Discrete_Range is
     limited interface and Program.Elements.Definitions.Definition;

   type Discrete_Range_Access is access all Discrete_Range'Class
     with Storage_Size => 0;

   type Discrete_Range_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Discrete_Range_Vector_Access is access all Discrete_Range_Vector'Class
     with Storage_Size => 0;

   overriding function Element
    (Self  : Discrete_Range_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Discrete_Range;

   function To_Discrete_Range
    (Self  : Discrete_Range_Vector'Class;
     Index : Positive)
      return not null Discrete_Range_Access
     is (Self.Element (Index).To_Discrete_Range);

end Program.Elements.Discrete_Ranges;
