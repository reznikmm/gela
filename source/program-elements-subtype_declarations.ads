--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Subtype_Declarations is

   pragma Pure (Program.Elements.Subtype_Declarations);

   type Subtype_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Subtype_Declaration_Access is access all Subtype_Declaration'Class
     with Storage_Size => 0;

end Program.Elements.Subtype_Declarations;
