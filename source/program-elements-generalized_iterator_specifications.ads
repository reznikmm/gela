--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Elements.Defining_Identifiers;
with Program.Tokens;
with Program.Elements.Expressions;

package Program.Elements.Generalized_Iterator_Specifications is

   pragma Pure (Program.Elements.Generalized_Iterator_Specifications);

   type Generalized_Iterator_Specification is
     limited interface and Program.Elements.Declarations.Declaration;

   type Generalized_Iterator_Specification_Access is
     access all Generalized_Iterator_Specification'Class
     with Storage_Size => 0;

   not overriding function Name
    (Self : Generalized_Iterator_Specification)
      return Program.Elements.Defining_Identifiers.Defining_Identifier_Access
     is abstract;

   not overriding function In_Token
    (Self : Generalized_Iterator_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Reverse_Token
    (Self : Generalized_Iterator_Specification)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Iterator_Name
    (Self : Generalized_Iterator_Specification)
      return Program.Elements.Expressions.Expression_Access is abstract;

end Program.Elements.Generalized_Iterator_Specifications;
