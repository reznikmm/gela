--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Entry_Body_Declarations is

   pragma Pure (Program.Elements.Entry_Body_Declarations);

   type Entry_Body_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Entry_Body_Declaration_Access is
     access all Entry_Body_Declaration'Class with Storage_Size => 0;

end Program.Elements.Entry_Body_Declarations;
