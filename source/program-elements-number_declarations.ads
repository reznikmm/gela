--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Number_Declarations is

   pragma Pure (Program.Elements.Number_Declarations);

   type Number_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Number_Declaration_Access is access all Number_Declaration'Class
     with Storage_Size => 0;

end Program.Elements.Number_Declarations;
