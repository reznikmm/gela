--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Aspect_Specifications;

package Program.Nodes.Aspect_Specification_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Aspect_Specifications
       .Aspect_Specification_Vector);
