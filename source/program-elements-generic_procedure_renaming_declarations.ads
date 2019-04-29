--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Generic_Procedure_Renaming_Declarations is

   pragma Pure (Program.Elements.Generic_Procedure_Renaming_Declarations);

   type Generic_Procedure_Renaming_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Generic_Procedure_Renaming_Declaration_Access is
     access all Generic_Procedure_Renaming_Declaration'Class
     with Storage_Size => 0;

end Program.Elements.Generic_Procedure_Renaming_Declarations;
