--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Single_Protected_Declarations is

   pragma Pure (Program.Elements.Single_Protected_Declarations);

   type Single_Protected_Declaration is
     limited interface and Program.Elements.Declarations.Declaration;

   type Single_Protected_Declaration_Access is
     access all Single_Protected_Declaration'Class with Storage_Size => 0;

end Program.Elements.Single_Protected_Declarations;
