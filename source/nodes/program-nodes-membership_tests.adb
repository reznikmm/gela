--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Membership_Tests is

   function Create
    (Expression : not null Program.Elements.Expressions.Expression_Access;
     Not_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     In_Token   : not null Program.Lexical_Elements.Lexical_Element_Access;
     Choices    : not null Program.Element_Vectors.Element_Vector_Access)
      return Membership_Test is
   begin
      return Result : Membership_Test :=
        (Expression => Expression, Not_Token => Not_Token,
         In_Token => In_Token, Choices => Choices, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Choices              : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Not              : Boolean := False)
      return Implicit_Membership_Test is
   begin
      return Result : Implicit_Membership_Test :=
        (Expression => Expression, Choices => Choices,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Has_Not => Has_Not,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Expression
    (Self : Base_Membership_Test)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Expression;
   end Expression;

   overriding function Choices
    (Self : Base_Membership_Test)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Choices;
   end Choices;

   overriding function Not_Token
    (Self : Membership_Test)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Not_Token;
   end Not_Token;

   overriding function In_Token
    (Self : Membership_Test)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.In_Token;
   end In_Token;

   overriding function Has_Not (Self : Membership_Test) return Boolean is
   begin
      return Self.Not_Token.Assigned;
   end Has_Not;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Membership_Test)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Membership_Test)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Membership_Test)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Not
    (Self : Implicit_Membership_Test)
      return Boolean is
   begin
      return Self.Has_Not;
   end Has_Not;

   procedure Initialize (Self : aliased in out Base_Membership_Test'Class) is
   begin
      Set_Enclosing_Element (Self.Expression, Self'Unchecked_Access);
      for Item in Self.Choices.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Membership_Test
    (Self : Base_Membership_Test)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Membership_Test;

   overriding function Is_Expression
    (Self : Base_Membership_Test)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression;

   overriding procedure Visit
    (Self    : not null access Base_Membership_Test;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Membership_Test (Self);
   end Visit;

   overriding function To_Membership_Test_Text
    (Self : aliased in out Membership_Test)
      return Program.Elements.Membership_Tests.Membership_Test_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Membership_Test_Text;

   overriding function To_Membership_Test_Text
    (Self : aliased in out Implicit_Membership_Test)
      return Program.Elements.Membership_Tests.Membership_Test_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Membership_Test_Text;

end Program.Nodes.Membership_Tests;
