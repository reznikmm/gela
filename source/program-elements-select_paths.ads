--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Paths;

package Program.Elements.Select_Paths is

   pragma Pure (Program.Elements.Select_Paths);

   type Select_Path is limited interface and Program.Elements.Paths.Path;

   type Select_Path_Access is access all Select_Path'Class
     with Storage_Size => 0;

end Program.Elements.Select_Paths;
