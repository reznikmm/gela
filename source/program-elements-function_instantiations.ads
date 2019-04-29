--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Function_Instantiations is

   pragma Pure (Program.Elements.Function_Instantiations);

   type Function_Instantiation is
     limited interface and Program.Elements.Declarations.Declaration;

   type Function_Instantiation_Access is
     access all Function_Instantiation'Class with Storage_Size => 0;

end Program.Elements.Function_Instantiations;
