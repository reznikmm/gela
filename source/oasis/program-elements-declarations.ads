--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package Program.Elements.Declarations is

   pragma Pure (Program.Elements.Declarations);

   type Declaration is limited interface and Program.Elements.Element;

   type Declaration_Access is access all Declaration'Class
     with Storage_Size => 0;

end Program.Elements.Declarations;
