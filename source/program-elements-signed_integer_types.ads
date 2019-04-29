--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Type_Definitions;

package Program.Elements.Signed_Integer_Types is

   pragma Pure (Program.Elements.Signed_Integer_Types);

   type Signed_Integer_Type is
     limited interface and Program.Elements.Type_Definitions.Type_Definition;

   type Signed_Integer_Type_Access is access all Signed_Integer_Type'Class
     with Storage_Size => 0;

end Program.Elements.Signed_Integer_Types;
