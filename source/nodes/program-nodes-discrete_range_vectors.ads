--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Discrete_Ranges;

package Program.Nodes.Discrete_Range_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Discrete_Ranges.Discrete_Range_Vector);
pragma Preelaborate (Program.Nodes.Discrete_Range_Vectors);
