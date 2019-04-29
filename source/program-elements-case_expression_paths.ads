--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Paths;

package Program.Elements.Case_Expression_Paths is

   pragma Pure (Program.Elements.Case_Expression_Paths);

   type Case_Expression_Path is
     limited interface and Program.Elements.Paths.Path;

   type Case_Expression_Path_Access is access all Case_Expression_Path'Class
     with Storage_Size => 0;

end Program.Elements.Case_Expression_Paths;
