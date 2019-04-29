--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Paths;

package Program.Elements.Then_Abort_Paths is

   pragma Pure (Program.Elements.Then_Abort_Paths);

   type Then_Abort_Path is limited interface and Program.Elements.Paths.Path;

   type Then_Abort_Path_Access is access all Then_Abort_Path'Class
     with Storage_Size => 0;

end Program.Elements.Then_Abort_Paths;
