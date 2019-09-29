--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Select_Paths;

package Program.Nodes.Select_Path_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Select_Paths.Select_Path_Vector);
