--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Case_Paths;

package Program.Nodes.Case_Path_Vectors is new
  Program.Nodes.Generic_Vectors (Program.Elements.Case_Paths.Case_Path_Vector);
pragma Preelaborate (Program.Nodes.Case_Path_Vectors);
