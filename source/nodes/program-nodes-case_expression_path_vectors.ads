--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Case_Expression_Paths;

package Program.Nodes.Case_Expression_Path_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Case_Expression_Paths
       .Case_Expression_Path_Vector);
