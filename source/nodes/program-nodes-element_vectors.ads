--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Element_Vectors;

package Program.Nodes.Element_Vectors is new
  Program.Nodes.Generic_Vectors (Program.Element_Vectors.Element_Vector);
