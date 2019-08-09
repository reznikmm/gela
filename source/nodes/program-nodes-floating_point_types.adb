--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Floating_Point_Types is

   function Create
    (Digits_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Digits_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range        : Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access)
      return Floating_Point_Type is
   begin
      return Result : Floating_Point_Type :=
        (Digits_Token => Digits_Token, Digits_Expression => Digits_Expression,
         Real_Range => Real_Range, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Digits_Expression    : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range           : Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Floating_Point_Type is
   begin
      return Result : Implicit_Floating_Point_Type :=
        (Digits_Expression => Digits_Expression, Real_Range => Real_Range,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Digits_Expression
    (Self : Base_Floating_Point_Type)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Digits_Expression;
   end Digits_Expression;

   overriding function Real_Range
    (Self : Base_Floating_Point_Type)
      return Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Access is
   begin
      return Self.Real_Range;
   end Real_Range;

   overriding function Digits_Token
    (Self : Floating_Point_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Digits_Token;
   end Digits_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Floating_Point_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Floating_Point_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Floating_Point_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Floating_Point_Type'Class) is
   begin
      Set_Enclosing_Element (Self.Digits_Expression, Self'Unchecked_Access);
      if Self.Real_Range.Assigned then
         Set_Enclosing_Element (Self.Real_Range, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Floating_Point_Type_Element
    (Self : Base_Floating_Point_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Floating_Point_Type_Element;

   overriding function Is_Type_Definition_Element
    (Self : Base_Floating_Point_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Type_Definition_Element;

   overriding function Is_Definition_Element
    (Self : Base_Floating_Point_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition_Element;

   overriding procedure Visit
    (Self    : not null access Base_Floating_Point_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Floating_Point_Type (Self);
   end Visit;

   overriding function To_Floating_Point_Type_Text
    (Self : aliased in out Floating_Point_Type)
      return Program.Elements.Floating_Point_Types
          .Floating_Point_Type_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Floating_Point_Type_Text;

   overriding function To_Floating_Point_Type_Text
    (Self : aliased in out Implicit_Floating_Point_Type)
      return Program.Elements.Floating_Point_Types
          .Floating_Point_Type_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Floating_Point_Type_Text;

end Program.Nodes.Floating_Point_Types;
