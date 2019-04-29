--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Slices is

   pragma Pure (Program.Elements.Slices);

   type Slice is limited interface and Program.Elements.Expressions.Expression;

   type Slice_Access is access all Slice'Class with Storage_Size => 0;

end Program.Elements.Slices;
