--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Object_Declarations is

   pragma Pure (Program.Elements.Object_Declarations);

   type Object_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Object_Declaration_Access is access all Object_Declaration'Class
     with Storage_Size => 0;

end Program.Elements.Object_Declarations;
