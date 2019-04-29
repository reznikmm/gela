--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;

package Program.Elements.Protected_Definitions is

   pragma Pure (Program.Elements.Protected_Definitions);

   type Protected_Definition is
     limited interface and Program.Elements.Definitions.Definition;

   type Protected_Definition_Access is access all Protected_Definition'Class
     with Storage_Size => 0;

end Program.Elements.Protected_Definitions;
