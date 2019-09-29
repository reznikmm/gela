--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Case_Paths;

package Program.Nodes.Case_Path_Vectors is new
  Program.Nodes.Generic_Vectors (Program.Elements.Case_Paths.Case_Path_Vector);
