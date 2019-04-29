--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Definitions;

package Program.Elements.Aspect_Specifications is

   pragma Pure (Program.Elements.Aspect_Specifications);

   type Aspect_Specification is
     limited interface and Program.Elements.Definitions.Definition;

   type Aspect_Specification_Access is access all Aspect_Specification'Class
     with Storage_Size => 0;

end Program.Elements.Aspect_Specifications;
