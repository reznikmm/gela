--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Discriminant_Specifications is

   pragma Pure (Program.Elements.Discriminant_Specifications);

   type Discriminant_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Discriminant_Specification_Access is
     access all Discriminant_Specification'Class with Storage_Size => 0;

end Program.Elements.Discriminant_Specifications;
