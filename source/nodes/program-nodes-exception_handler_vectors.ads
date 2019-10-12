--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Exception_Handlers;

package Program.Nodes.Exception_Handler_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Exception_Handlers.Exception_Handler_Vector);
pragma Preelaborate (Program.Nodes.Exception_Handler_Vectors);
