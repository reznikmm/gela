--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Paths;

package Program.Elements.Elsif_Paths is

   pragma Pure (Program.Elements.Elsif_Paths);

   type Elsif_Path is limited interface and Program.Elements.Paths.Path;

   type Elsif_Path_Access is access all Elsif_Path'Class
     with Storage_Size => 0;

end Program.Elements.Elsif_Paths;
