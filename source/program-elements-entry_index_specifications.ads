--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Entry_Index_Specifications is

   pragma Pure (Program.Elements.Entry_Index_Specifications);

   type Entry_Index_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Entry_Index_Specification_Access is
     access all Entry_Index_Specification'Class with Storage_Size => 0;

end Program.Elements.Entry_Index_Specifications;
