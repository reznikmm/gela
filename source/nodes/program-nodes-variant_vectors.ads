--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Variants;

package Program.Nodes.Variant_Vectors is new
  Program.Nodes.Generic_Vectors (Program.Elements.Variants.Variant_Vector);
