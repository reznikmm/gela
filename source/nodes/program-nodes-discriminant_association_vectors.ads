--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Discriminant_Associations;

package Program.Nodes.Discriminant_Association_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Discriminant_Associations
       .Discriminant_Association_Vector);
pragma Preelaborate (Program.Nodes.Discriminant_Association_Vectors);
