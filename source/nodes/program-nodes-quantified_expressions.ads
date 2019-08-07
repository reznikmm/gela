--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Program.Lexical_Elements;
with Program.Elements.Loop_Parameter_Specifications;
with Program.Elements.Generalized_Iterator_Specifications;
with Program.Elements.Element_Iterator_Specifications;
with Program.Elements.Expressions;
with Program.Elements.Quantified_Expressions;
with Program.Element_Visitors;

package Program.Nodes.Quantified_Expressions is

   pragma Preelaborate;

   type Quantified_Expression is
     new Program.Nodes.Node
         and Program.Elements.Quantified_Expressions.Quantified_Expression
         and Program.Elements.Quantified_Expressions.Quantified_Expression_Text
     with private;

   function Create
    (For_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     All_Token            : Program.Lexical_Elements.Lexical_Element_Access;
     Some_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Parameter            : Program.Elements.Loop_Parameter_Specifications
         .Loop_Parameter_Specification_Access;
     Generalized_Iterator : Program.Elements
         .Generalized_Iterator_Specifications
         .Generalized_Iterator_Specification_Access;
     Element_Iterator     : Program.Elements.Element_Iterator_Specifications
         .Element_Iterator_Specification_Access;
     Arrow_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Predicate            : not null Program.Elements.Expressions
         .Expression_Access)
      return Quantified_Expression;

   type Implicit_Quantified_Expression is
     new Program.Nodes.Node
         and Program.Elements.Quantified_Expressions.Quantified_Expression
     with private;

   function Create
    (Parameter            : Program.Elements.Loop_Parameter_Specifications
         .Loop_Parameter_Specification_Access;
     Generalized_Iterator : Program.Elements
         .Generalized_Iterator_Specifications
         .Generalized_Iterator_Specification_Access;
     Element_Iterator     : Program.Elements.Element_Iterator_Specifications
         .Element_Iterator_Specification_Access;
     Predicate            : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_All              : Boolean := False;
     Has_Some             : Boolean := False)
      return Implicit_Quantified_Expression
     with Pre =>
       Is_Part_Of_Implicit or Is_Part_Of_Inherited or Is_Part_Of_Instance;
private

   type Base_Quantified_Expression is
     abstract new Program.Nodes.Node
       and Program.Elements.Quantified_Expressions.Quantified_Expression
     with record
        Parameter            : Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification_Access;
        Generalized_Iterator : Program.Elements
          .Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification_Access;
        Element_Iterator     : Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification_Access;
        Predicate            : not null Program.Elements.Expressions
          .Expression_Access;
     end record;

   procedure Initialize
    (Self : aliased in out Base_Quantified_Expression'Class);

   overriding procedure Visit
    (Self    : not null access Base_Quantified_Expression;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class);

   overriding function Parameter
    (Self : Base_Quantified_Expression)
      return Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification_Access;

   overriding function Generalized_Iterator
    (Self : Base_Quantified_Expression)
      return Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification_Access;

   overriding function Element_Iterator
    (Self : Base_Quantified_Expression)
      return Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification_Access;

   overriding function Predicate
    (Self : Base_Quantified_Expression)
      return not null Program.Elements.Expressions.Expression_Access;

   overriding function Is_Quantified_Expression
    (Self : Base_Quantified_Expression)
      return Boolean;

   overriding function Is_Expression
    (Self : Base_Quantified_Expression)
      return Boolean;

   type Quantified_Expression is
     new Base_Quantified_Expression
       and Program.Elements.Quantified_Expressions.Quantified_Expression_Text
     with record
        For_Token   : not null Program.Lexical_Elements.Lexical_Element_Access;
        All_Token   : Program.Lexical_Elements.Lexical_Element_Access;
        Some_Token  : Program.Lexical_Elements.Lexical_Element_Access;
        Arrow_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     end record;

   overriding function To_Quantified_Expression_Text
    (Self : aliased in out Quantified_Expression)
      return Program.Elements.Quantified_Expressions
          .Quantified_Expression_Text_Access;

   overriding function For_Token
    (Self : Quantified_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function All_Token
    (Self : Quantified_Expression)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Some_Token
    (Self : Quantified_Expression)
      return Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Arrow_Token
    (Self : Quantified_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access;

   overriding function Has_All (Self : Quantified_Expression) return Boolean;

   overriding function Has_Some (Self : Quantified_Expression) return Boolean;

   type Implicit_Quantified_Expression is
     new Base_Quantified_Expression
     with record
        Is_Part_Of_Implicit  : Boolean;
        Is_Part_Of_Inherited : Boolean;
        Is_Part_Of_Instance  : Boolean;
        Has_All              : Boolean;
        Has_Some             : Boolean;
     end record;

   overriding function To_Quantified_Expression_Text
    (Self : aliased in out Implicit_Quantified_Expression)
      return Program.Elements.Quantified_Expressions
          .Quantified_Expression_Text_Access;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Quantified_Expression)
      return Boolean;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Quantified_Expression)
      return Boolean;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Quantified_Expression)
      return Boolean;

   overriding function Has_All
    (Self : Implicit_Quantified_Expression)
      return Boolean;

   overriding function Has_Some
    (Self : Implicit_Quantified_Expression)
      return Boolean;

end Program.Nodes.Quantified_Expressions;
