--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Generalized_Iterator_Specifications is

   pragma Pure (Program.Elements.Generalized_Iterator_Specifications);

   type Generalized_Iterator_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Generalized_Iterator_Specification_Access is
     access all Generalized_Iterator_Specification'Class
     with Storage_Size => 0;

end Program.Elements.Generalized_Iterator_Specifications;
