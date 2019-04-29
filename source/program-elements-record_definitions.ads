--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;

package Program.Elements.Record_Definitions is

   pragma Pure (Program.Elements.Record_Definitions);

   type Record_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Record_Definition_Access is access all Record_Definition'Class
     with Storage_Size => 0;

end Program.Elements.Record_Definitions;
