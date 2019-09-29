--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Record_Component_Associations;

package Program.Nodes.Record_Component_Association_Vectors is new
  Program.Nodes.Generic_Vectors
    (Program.Elements.Record_Component_Associations
       .Record_Component_Association_Vector);
