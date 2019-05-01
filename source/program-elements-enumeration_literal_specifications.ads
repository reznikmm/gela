--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Elements.Defining_Identifiers;

package Program.Elements.Enumeration_Literal_Specifications is

   pragma Pure (Program.Elements.Enumeration_Literal_Specifications);

   type Enumeration_Literal_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Enumeration_Literal_Specification_Access is
     access all Enumeration_Literal_Specification'Class with Storage_Size => 0;

   not overriding function Name
    (Self : Enumeration_Literal_Specification)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
     is abstract;

end Program.Elements.Enumeration_Literal_Specifications;
