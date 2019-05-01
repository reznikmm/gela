--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Tokens;
with Program.Elements.Defining_Identifiers;
with Program.Elements.Discrete_Subtype_Definitions;

package Program.Elements.Entry_Index_Specifications is

   pragma Pure (Program.Elements.Entry_Index_Specifications);

   type Entry_Index_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Entry_Index_Specification_Access is
     access all Entry_Index_Specification'Class with Storage_Size => 0;

   not overriding function For_Token
    (Self : Entry_Index_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Name
    (Self : Entry_Index_Specification)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
     is abstract;

   not overriding function In_Token
    (Self : Entry_Index_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Subtype_Definition
    (Self : Entry_Index_Specification)
      return Program.Elements.Discrete_Subtype_Definitions
          .Discrete_Subtype_Definition_Access is abstract;

end Program.Elements.Entry_Index_Specifications;
