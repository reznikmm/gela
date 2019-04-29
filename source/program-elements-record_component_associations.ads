--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Associations;

package Program.Elements.Record_Component_Associations is

   pragma Pure (Program.Elements.Record_Component_Associations);

   type Record_Component_Association is
     limited interface and Program.Elements.Associations.Association;

   type Record_Component_Association_Access is
     access all Record_Component_Association'Class with Storage_Size => 0;

end Program.Elements.Record_Component_Associations;
