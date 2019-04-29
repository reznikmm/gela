--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Return_Object_Specifications is

   pragma Pure (Program.Elements.Return_Object_Specifications);

   type Return_Object_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Return_Object_Specification_Access is
     access all Return_Object_Specification'Class with Storage_Size => 0;

end Program.Elements.Return_Object_Specifications;
