--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

with Program.Nodes.Generic_Vectors;
with Program.Elements.Variants;

package Program.Nodes.Variant_Vectors is new
  Program.Nodes.Generic_Vectors (Program.Elements.Variants.Variant_Vector);
pragma Preelaborate (Program.Nodes.Variant_Vectors);

