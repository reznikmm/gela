--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Procedure_Declarations is

   pragma Pure (Program.Elements.Procedure_Declarations);

   type Procedure_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Procedure_Declaration_Access is access all Procedure_Declaration'Class
     with Storage_Size => 0;

end Program.Elements.Procedure_Declarations;
