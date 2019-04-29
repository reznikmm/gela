--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Program.Elements.Associations is

   pragma Pure (Program.Elements.Associations);

   type Association is limited interface and Program.Elements.Element;

   type Association_Access is access all Association'Class
     with Storage_Size => 0;

end Program.Elements.Associations;
