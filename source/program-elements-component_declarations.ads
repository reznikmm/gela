--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Component_Declarations is

   pragma Pure (Program.Elements.Component_Declarations);

   type Component_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Component_Declaration_Access is access all Component_Declaration'Class
     with Storage_Size => 0;

end Program.Elements.Component_Declarations;
