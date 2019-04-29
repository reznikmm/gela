--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Loop_Parameter_Specifications is

   pragma Pure (Program.Elements.Loop_Parameter_Specifications);

   type Loop_Parameter_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Loop_Parameter_Specification_Access is
     access all Loop_Parameter_Specification'Class with Storage_Size => 0;

end Program.Elements.Loop_Parameter_Specifications;
