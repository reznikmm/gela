--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Ordinary_Fixed_Point_Types is

   function Create
    (Delta_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Delta_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range       : not null Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access)
      return Ordinary_Fixed_Point_Type is
   begin
      return Result : Ordinary_Fixed_Point_Type :=
        (Delta_Token => Delta_Token, Delta_Expression => Delta_Expression,
         Real_Range => Real_Range, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Delta_Expression     : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range           : not null Program.Elements.Real_Range_Specifications
         .Real_Range_Specification_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Ordinary_Fixed_Point_Type is
   begin
      return Result : Implicit_Ordinary_Fixed_Point_Type :=
        (Delta_Expression => Delta_Expression, Real_Range => Real_Range,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Delta_Expression
    (Self : Base_Ordinary_Fixed_Point_Type)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Delta_Expression;
   end Delta_Expression;

   overriding function Real_Range
    (Self : Base_Ordinary_Fixed_Point_Type)
      return not null Program.Elements.Real_Range_Specifications
          .Real_Range_Specification_Access is
   begin
      return Self.Real_Range;
   end Real_Range;

   overriding function Delta_Token
    (Self : Ordinary_Fixed_Point_Type)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Delta_Token;
   end Delta_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Ordinary_Fixed_Point_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Ordinary_Fixed_Point_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Ordinary_Fixed_Point_Type)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Ordinary_Fixed_Point_Type'Class) is
   begin
      Set_Enclosing_Element (Self.Delta_Expression, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Real_Range, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Ordinary_Fixed_Point_Type
    (Self : Base_Ordinary_Fixed_Point_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Ordinary_Fixed_Point_Type;

   overriding function Is_Type_Definition
    (Self : Base_Ordinary_Fixed_Point_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Type_Definition;

   overriding function Is_Definition
    (Self : Base_Ordinary_Fixed_Point_Type)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Ordinary_Fixed_Point_Type;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Ordinary_Fixed_Point_Type (Self);
   end Visit;

   overriding function To_Ordinary_Fixed_Point_Type_Text
    (Self : aliased in out Ordinary_Fixed_Point_Type)
      return Program.Elements.Ordinary_Fixed_Point_Types
          .Ordinary_Fixed_Point_Type_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Ordinary_Fixed_Point_Type_Text;

   overriding function To_Ordinary_Fixed_Point_Type_Text
    (Self : aliased in out Implicit_Ordinary_Fixed_Point_Type)
      return Program.Elements.Ordinary_Fixed_Point_Types
          .Ordinary_Fixed_Point_Type_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Ordinary_Fixed_Point_Type_Text;

end Program.Nodes.Ordinary_Fixed_Point_Types;
