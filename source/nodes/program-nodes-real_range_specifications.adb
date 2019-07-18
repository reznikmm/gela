--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Real_Range_Specifications is

   function Create
    (Range_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Lower_Bound      : not null Program.Elements.Expressions
         .Expression_Access;
     Double_Dot_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Upper_Bound      : not null Program.Elements.Expressions
         .Expression_Access)
      return Real_Range_Specification is
   begin
      return Result : Real_Range_Specification :=
        (Range_Token => Range_Token, Lower_Bound => Lower_Bound,
         Double_Dot_Token => Double_Dot_Token, Upper_Bound => Upper_Bound,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Lower_Bound          : not null Program.Elements.Expressions
         .Expression_Access;
     Upper_Bound          : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Real_Range_Specification is
   begin
      return Result : Implicit_Real_Range_Specification :=
        (Lower_Bound => Lower_Bound, Upper_Bound => Upper_Bound,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Lower_Bound
    (Self : Base_Real_Range_Specification)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Lower_Bound;
   end Lower_Bound;

   overriding function Upper_Bound
    (Self : Base_Real_Range_Specification)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Upper_Bound;
   end Upper_Bound;

   overriding function Range_Token
    (Self : Real_Range_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Range_Token;
   end Range_Token;

   overriding function Double_Dot_Token
    (Self : Real_Range_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Double_Dot_Token;
   end Double_Dot_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Real_Range_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Real_Range_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Real_Range_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Real_Range_Specification'Class) is
   begin
      Set_Enclosing_Element (Self.Lower_Bound, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Upper_Bound, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Real_Range_Specification
    (Self : Base_Real_Range_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Real_Range_Specification;

   overriding function Is_Definition
    (Self : Base_Real_Range_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Real_Range_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Real_Range_Specification (Self);
   end Visit;

   overriding function To_Real_Range_Specification_Text
    (Self : aliased in out Real_Range_Specification)
      return Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Real_Range_Specification_Text;

   overriding function To_Real_Range_Specification_Text
    (Self : aliased in out Implicit_Real_Range_Specification)
      return Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Real_Range_Specification_Text;

end Program.Nodes.Real_Range_Specifications;
