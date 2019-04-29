--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Associations;

package Program.Elements.Discriminant_Associations is

   pragma Pure (Program.Elements.Discriminant_Associations);

   type Discriminant_Association is
     limited interface and Program.Elements.Associations.Association;

   type Discriminant_Association_Access is
     access all Discriminant_Association'Class with Storage_Size => 0;

end Program.Elements.Discriminant_Associations;
