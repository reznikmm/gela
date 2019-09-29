--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Discriminant_Specifications;

package Program.Nodes.Discriminant_Specification_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Discriminant_Specifications
       .Discriminant_Specification_Vector);
