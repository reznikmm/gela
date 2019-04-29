--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Function_Renaming_Declarations is

   pragma Pure (Program.Elements.Function_Renaming_Declarations);

   type Function_Renaming_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Function_Renaming_Declaration_Access is
     access all Function_Renaming_Declaration'Class with Storage_Size => 0;

end Program.Elements.Function_Renaming_Declarations;