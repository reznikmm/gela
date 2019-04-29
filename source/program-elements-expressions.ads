--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Program.Elements.Expressions is

   pragma Pure (Program.Elements.Expressions);

   type Expression is limited interface and Program.Elements.Element;

   type Expression_Access is access all Expression'Class
     with Storage_Size => 0;

end Program.Elements.Expressions;
