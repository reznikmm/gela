--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Component_Clauses;

package Program.Nodes.Component_Clause_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Component_Clauses.Component_Clause_Vector);
pragma Preelaborate (Program.Nodes.Component_Clause_Vectors);
