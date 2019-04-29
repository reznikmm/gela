--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Associations;

package Program.Elements.Parameter_Associations is

   pragma Pure (Program.Elements.Parameter_Associations);

   type Parameter_Association is
     limited interface and Program.Elements.Associations.Association;

   type Parameter_Association_Access is access all Parameter_Association'Class
     with Storage_Size => 0;

end Program.Elements.Parameter_Associations;
