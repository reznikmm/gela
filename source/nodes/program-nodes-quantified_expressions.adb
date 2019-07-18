--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Quantified_Expressions is

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
      return Quantified_Expression is
   begin
      return Result : Quantified_Expression :=
        (For_Token => For_Token, All_Token => All_Token,
         Some_Token => Some_Token, Parameter => Parameter,
         Generalized_Iterator => Generalized_Iterator,
         Element_Iterator => Element_Iterator, Arrow_Token => Arrow_Token,
         Predicate => Predicate, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

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
      return Implicit_Quantified_Expression is
   begin
      return Result : Implicit_Quantified_Expression :=
        (Parameter => Parameter, Generalized_Iterator => Generalized_Iterator,
         Element_Iterator => Element_Iterator, Predicate => Predicate,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Has_All => Has_All,
         Has_Some => Has_Some, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Parameter
    (Self : Base_Quantified_Expression)
      return Program.Elements.Loop_Parameter_Specifications
          .Loop_Parameter_Specification_Access is
   begin
      return Self.Parameter;
   end Parameter;

   overriding function Generalized_Iterator
    (Self : Base_Quantified_Expression)
      return Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification_Access is
   begin
      return Self.Generalized_Iterator;
   end Generalized_Iterator;

   overriding function Element_Iterator
    (Self : Base_Quantified_Expression)
      return Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification_Access is
   begin
      return Self.Element_Iterator;
   end Element_Iterator;

   overriding function Predicate
    (Self : Base_Quantified_Expression)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Predicate;
   end Predicate;

   overriding function For_Token
    (Self : Quantified_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.For_Token;
   end For_Token;

   overriding function All_Token
    (Self : Quantified_Expression)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.All_Token;
   end All_Token;

   overriding function Some_Token
    (Self : Quantified_Expression)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Some_Token;
   end Some_Token;

   overriding function Arrow_Token
    (Self : Quantified_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Arrow_Token;
   end Arrow_Token;

   overriding function Has_All (Self : Quantified_Expression) return Boolean is
   begin
      return Self.All_Token.Assigned;
   end Has_All;

   overriding function Has_Some
    (Self : Quantified_Expression)
      return Boolean is
   begin
      return Self.Some_Token.Assigned;
   end Has_Some;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Quantified_Expression)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Quantified_Expression)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Quantified_Expression)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_All
    (Self : Implicit_Quantified_Expression)
      return Boolean is
   begin
      return Self.Has_All;
   end Has_All;

   overriding function Has_Some
    (Self : Implicit_Quantified_Expression)
      return Boolean is
   begin
      return Self.Has_Some;
   end Has_Some;

   procedure Initialize
    (Self : aliased in out Base_Quantified_Expression'Class) is
   begin
      if Self.Parameter.Assigned then
         Set_Enclosing_Element (Self.Parameter, Self'Unchecked_Access);
      end if;
      if Self.Generalized_Iterator.Assigned then
         Set_Enclosing_Element
           (Self.Generalized_Iterator, Self'Unchecked_Access);
      end if;
      if Self.Element_Iterator.Assigned then
         Set_Enclosing_Element (Self.Element_Iterator, Self'Unchecked_Access);
      end if;
      Set_Enclosing_Element (Self.Predicate, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Quantified_Expression
    (Self : Base_Quantified_Expression)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Quantified_Expression;

   overriding function Is_Expression
    (Self : Base_Quantified_Expression)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression;

   overriding procedure Visit
    (Self    : not null access Base_Quantified_Expression;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Quantified_Expression (Self);
   end Visit;

   overriding function To_Quantified_Expression_Text
    (Self : aliased in out Quantified_Expression)
      return Program.Elements.Quantified_Expressions
          .Quantified_Expression_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Quantified_Expression_Text;

   overriding function To_Quantified_Expression_Text
    (Self : aliased in out Implicit_Quantified_Expression)
      return Program.Elements.Quantified_Expressions
          .Quantified_Expression_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Quantified_Expression_Text;

end Program.Nodes.Quantified_Expressions;
