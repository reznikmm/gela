--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;

package Program.Elements.Allocators is

   pragma Pure (Program.Elements.Allocators);

   type Allocator is
     limited interface and Program.Elements.Expressions.Expression;

   type Allocator_Access is access all Allocator'Class with Storage_Size => 0;

end Program.Elements.Allocators;
