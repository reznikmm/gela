--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;

package Program.Elements.Record_Types is

   pragma Pure (Program.Elements.Record_Types);

   type Record_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Record_Type_Access is access all Record_Type'Class
     with Storage_Size => 0;

end Program.Elements.Record_Types;
