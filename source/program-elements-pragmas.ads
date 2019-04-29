--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Program.Elements.Pragmas is

   pragma Pure (Program.Elements.Pragmas);

   type Pragma_Element is limited interface and Program.Elements.Element;

   type Pragma_Access is access all Pragma_Element'Class
     with Storage_Size => 0;

end Program.Elements.Pragmas;
