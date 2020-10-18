--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Discriminant_Specifications;

package Program.Nodes.Discriminant_Specification_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Discriminant_Specifications
       .Discriminant_Specification_Vector);
pragma Preelaborate (Program.Nodes.Discriminant_Specification_Vectors);
