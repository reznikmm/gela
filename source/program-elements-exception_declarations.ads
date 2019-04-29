--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Exception_Declarations is

   pragma Pure (Program.Elements.Exception_Declarations);

   type Exception_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Exception_Declaration_Access is access all Exception_Declaration'Class
     with Storage_Size => 0;

end Program.Elements.Exception_Declarations;
