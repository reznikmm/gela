--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package Program.Elements.Definitions is

   pragma Pure (Program.Elements.Definitions);

   type Definition is limited interface and Program.Elements.Element;

   type Definition_Access is access all Definition'Class
     with Storage_Size => 0;

end Program.Elements.Definitions;
