--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Elements.Defining_Identifiers;
with Program.Tokens;
with Program.Elements.Subtype_Indications;
with Program.Elements.Expressions;

package Program.Elements.Element_Iterator_Specifications is

   pragma Pure (Program.Elements.Element_Iterator_Specifications);

   type Element_Iterator_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Element_Iterator_Specification_Access is
     access all Element_Iterator_Specification'Class with Storage_Size => 0;

   not overriding function Name
    (Self : Element_Iterator_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function Colon_Token
    (Self : Element_Iterator_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Subtype_Indication
    (Self : Element_Iterator_Specification)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access is abstract;

   not overriding function Of_Token
    (Self : Element_Iterator_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Reverse_Token
    (Self : Element_Iterator_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Iterable_Name
    (Self : Element_Iterator_Specification)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

end Program.Elements.Element_Iterator_Specifications;
