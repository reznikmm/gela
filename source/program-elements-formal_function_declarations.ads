--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Formal_Function_Declarations is

   pragma Pure (Program.Elements.Formal_Function_Declarations);

   type Formal_Function_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Formal_Function_Declaration_Access is
     access all Formal_Function_Declaration'Class with Storage_Size => 0;

end Program.Elements.Formal_Function_Declarations;
