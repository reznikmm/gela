--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Access_Types;
with Program.Elements.Formal_Access_Types;
with Program.Elements.Anonymous_Access_Definitions;

package Program.Elements.Procedure_Access_Types is

   pragma Pure (Program.Elements.Procedure_Access_Types);

   type Procedure_Access_Type is
     limited interface and Program.Elements.Access_Types.Access_Type
       and Program.Elements.Formal_Access_Types.Formal_Access_Type
       and Program.Elements.Anonymous_Access_Definitions
         .Anonymous_Access_Definition;

   type Procedure_Access_Type_Access is access all Procedure_Access_Type'Class
     with Storage_Size => 0;

end Program.Elements.Procedure_Access_Types;