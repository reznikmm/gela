--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Elsif_Paths;

package Program.Nodes.Elsif_Path_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Elsif_Paths.Elsif_Path_Vector);
