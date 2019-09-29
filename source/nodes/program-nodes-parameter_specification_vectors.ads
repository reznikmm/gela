--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Parameter_Specifications;

package Program.Nodes.Parameter_Specification_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Parameter_Specifications.Parameter_Specification_Vector);
