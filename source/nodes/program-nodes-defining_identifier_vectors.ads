--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Defining_Identifiers;

package Program.Nodes.Defining_Identifier_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Defining_Identifiers.Defining_Identifier_Vector);
pragma Preelaborate (Program.Nodes.Defining_Identifier_Vectors);
