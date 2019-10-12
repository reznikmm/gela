--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Expressions;

package Program.Nodes.Expression_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Expressions.Expression_Vector);
pragma Preelaborate (Program.Nodes.Expression_Vectors);
