--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Enumeration_Literal_Specifications;

package Program.Nodes.Enumeration_Literal_Specification_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Enumeration_Literal_Specifications
       .Enumeration_Literal_Specification_Vector);
pragma Preelaborate (Program.Nodes.Enumeration_Literal_Specification_Vectors);
