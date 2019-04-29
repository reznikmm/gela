--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;

package Program.Elements.Element_Iterator_Specifications is

   pragma Pure (Program.Elements.Element_Iterator_Specifications);

   type Element_Iterator_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Element_Iterator_Specification_Access is
     access all Element_Iterator_Specification'Class with Storage_Size => 0;

end Program.Elements.Element_Iterator_Specifications;
