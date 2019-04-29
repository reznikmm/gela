--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Type_Declarations is

   pragma Pure (Program.Elements.Type_Declarations);

   type Type_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Type_Declaration_Access is access all Type_Declaration'Class
     with Storage_Size => 0;

end Program.Elements.Type_Declarations;
