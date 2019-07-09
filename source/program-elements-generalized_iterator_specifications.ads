--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Declarations;
with Program.Elements.Defining_Identifiers;
with Program.Lexical_Elements;
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
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is abstract;

   not overriding function Iterator_Name
    (Self : Generalized_Iterator_Specification)
      return not null Program.Elements.Expressions.Expression_Access
     is abstract;

   type Generalized_Iterator_Specification_Text is limited interface;

   type Generalized_Iterator_Specification_Text_Access is
     access all Generalized_Iterator_Specification_Text'Class
     with Storage_Size => 0;

   not overriding function To_Generalized_Iterator_Specification_Text
    (Self : aliased Generalized_Iterator_Specification)
      return Generalized_Iterator_Specification_Text_Access is abstract;

   not overriding function In_Token
    (Self : Generalized_Iterator_Specification_Text)
      return not null Program.Lexical_Elements.Lexical_Element_Access
     is abstract;

   not overriding function Reverse_Token
    (Self : Generalized_Iterator_Specification_Text)
      return Program.Lexical_Elements.Lexical_Element_Access is abstract;

end Program.Elements.Generalized_Iterator_Specifications;
