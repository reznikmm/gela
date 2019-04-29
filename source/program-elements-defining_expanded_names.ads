--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Defining_Names;

package Program.Elements.Defining_Expanded_Names is

   pragma Pure (Program.Elements.Defining_Expanded_Names);

   type Defining_Expanded_Name is
     limited interface and Program.Elements.Defining_Names.Defining_Name;

   type Defining_Expanded_Name_Access is
     access all Defining_Expanded_Name'Class with Storage_Size => 0;

end Program.Elements.Defining_Expanded_Names;
