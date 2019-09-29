--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Formal_Package_Associations;

package Program.Nodes.Formal_Package_Association_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Formal_Package_Associations
       .Formal_Package_Association_Vector);
