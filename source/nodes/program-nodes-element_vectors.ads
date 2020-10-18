--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Element_Vectors;

package Program.Nodes.Element_Vectors is new
  Program.Nodes.Generic_Vectors (Program.Element_Vectors.Element_Vector);
pragma Preelaborate (Program.Nodes.Element_Vectors);
