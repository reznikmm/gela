--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Parameter_Specifications is

   pragma Pure (Program.Elements.Parameter_Specifications);

   type Parameter_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Parameter_Specification_Access is
     access all Parameter_Specification'Class with Storage_Size => 0;

end Program.Elements.Parameter_Specifications;
