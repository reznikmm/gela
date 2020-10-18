--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Array_Component_Associations;

package Program.Nodes.Array_Component_Association_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Array_Component_Associations
       .Array_Component_Association_Vector);
pragma Preelaborate (Program.Nodes.Array_Component_Association_Vectors);
