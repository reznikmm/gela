--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package Program.Elements.Paths is

   pragma Pure (Program.Elements.Paths);

   type Path is limited interface and Program.Elements.Element;

   type Path_Access is access all Path'Class with Storage_Size => 0;

end Program.Elements.Paths;
