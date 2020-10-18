--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Parameter_Associations;

package Program.Nodes.Parameter_Association_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Parameter_Associations.Parameter_Association_Vector);
pragma Preelaborate (Program.Nodes.Parameter_Association_Vectors);
