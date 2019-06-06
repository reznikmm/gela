--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;
with Program.Element_Vectors;

package Program.Elements.Discrete_Subtype_Definitions is

   pragma Pure (Program.Elements.Discrete_Subtype_Definitions);

   type Discrete_Subtype_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Discrete_Subtype_Definition_Access is
     access all Discrete_Subtype_Definition'Class with Storage_Size => 0;

   type Discrete_Subtype_Definition_Vector is
     limited interface and Program.Element_Vectors.Element_Vector;

   type Discrete_Subtype_Definition_Vector_Access is
     access all Discrete_Subtype_Definition_Vector'Class
     with Storage_Size => 0;

   overriding function Element
    (Self  : Discrete_Subtype_Definition_Vector;
     Index : Positive)
      return not null Program.Elements.Element_Access is abstract
     with Post'Class => Element'Result.Is_Discrete_Subtype_Definition;

   function To_Discrete_Subtype_Definition
    (Self  : Discrete_Subtype_Definition_Vector'Class;
     Index : Positive)
      return not null Discrete_Subtype_Definition_Access
     is (Self.Element (Index).To_Discrete_Subtype_Definition);

end Program.Elements.Discrete_Subtype_Definitions;
