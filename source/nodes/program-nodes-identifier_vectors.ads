--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Identifiers;

package Program.Nodes.Identifier_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Identifiers.Identifier_Vector);
pragma Preelaborate (Program.Nodes.Identifier_Vectors);
