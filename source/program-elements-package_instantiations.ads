--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Package_Instantiations is

   pragma Pure (Program.Elements.Package_Instantiations);

   type Package_Instantiation is
     limited interface and Program.Elements.Declarations.Declaration;

   type Package_Instantiation_Access is access all Package_Instantiation'Class
     with Storage_Size => 0;

end Program.Elements.Package_Instantiations;
