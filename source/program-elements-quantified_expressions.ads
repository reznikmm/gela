--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Elements.Expressions;
with Program.Tokens;
with Program.Elements.Loop_Parameter_Specifications;
with Program.Elements.Generalized_Iterator_Specifications;
with Program.Elements.Element_Iterator_Specifications;

package Program.Elements.Quantified_Expressions is

   pragma Pure (Program.Elements.Quantified_Expressions);

   type Quantified_Expression is
     limited interface and Program.Elements.Expressions.Expression;

   type Quantified_Expression_Access is access all Quantified_Expression'Class
     with Storage_Size => 0;

   not overriding function For_Token
    (Self : Quantified_Expression)
      return Program.Tokens.Token_Access is abstract;

   not overriding function All_Token
    (Self : Quantified_Expression)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Some_Token
    (Self : Quantified_Expression)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Loop_Parameter_Specification
    (Self : Quantified_Expression)
      return Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification_Access is abstract;

   not overriding function Generalized_Iterator
    (Self : Quantified_Expression)
      return Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification_Access is abstract;

   not overriding function Element_Iterator
    (Self : Quantified_Expression)
      return Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification_Access is abstract;

   not overriding function Arrow_Token
    (Self : Quantified_Expression)
      return Program.Tokens.Token_Access is abstract;

   not overriding function Predicate
    (Self : Quantified_Expression)
      return Program.Elements.Expressions.Expression_Access is abstract;

end Program.Elements.Quantified_Expressions;
