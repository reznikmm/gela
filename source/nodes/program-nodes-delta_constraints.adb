--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Delta_Constraints is

   function Create
    (Delta_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Delta_Expression      : not null Program.Elements.Expressions
         .Expression_Access;
     Range_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Real_Range_Constraint : Program.Elements.Constraints.Constraint_Access)
      return Delta_Constraint is
   begin
      return Result : Delta_Constraint :=
        (Delta_Token => Delta_Token, Delta_Expression => Delta_Expression,
         Range_Token => Range_Token,
         Real_Range_Constraint => Real_Range_Constraint,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Delta_Expression      : not null Program.Elements.Expressions
         .Expression_Access;
     Real_Range_Constraint : Program.Elements.Constraints.Constraint_Access;
     Is_Part_Of_Implicit   : Boolean := False;
     Is_Part_Of_Inherited  : Boolean := False;
     Is_Part_Of_Instance   : Boolean := False)
      return Implicit_Delta_Constraint is
   begin
      return Result : Implicit_Delta_Constraint :=
        (Delta_Expression => Delta_Expression,
         Real_Range_Constraint => Real_Range_Constraint,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Delta_Expression
    (Self : Base_Delta_Constraint)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Delta_Expression;
   end Delta_Expression;

   overriding function Real_Range_Constraint
    (Self : Base_Delta_Constraint)
      return Program.Elements.Constraints.Constraint_Access is
   begin
      return Self.Real_Range_Constraint;
   end Real_Range_Constraint;

   overriding function Delta_Token
    (Self : Delta_Constraint)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Delta_Token;
   end Delta_Token;

   overriding function Range_Token
    (Self : Delta_Constraint)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Range_Token;
   end Range_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Delta_Constraint)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Delta_Constraint)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Delta_Constraint)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Delta_Constraint'Class) is
   begin
      Set_Enclosing_Element (Self.Delta_Expression, Self'Unchecked_Access);
      if Self.Real_Range_Constraint.Assigned then
         Set_Enclosing_Element
           (Self.Real_Range_Constraint, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Delta_Constraint
    (Self : Base_Delta_Constraint)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Delta_Constraint;

   overriding function Is_Constraint
    (Self : Base_Delta_Constraint)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Constraint;

   overriding function Is_Definition
    (Self : Base_Delta_Constraint)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Delta_Constraint;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Delta_Constraint (Self);
   end Visit;

   overriding function To_Delta_Constraint_Text
    (Self : aliased in out Delta_Constraint)
      return Program.Elements.Delta_Constraints.Delta_Constraint_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Delta_Constraint_Text;

   overriding function To_Delta_Constraint_Text
    (Self : aliased in out Implicit_Delta_Constraint)
      return Program.Elements.Delta_Constraints.Delta_Constraint_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Delta_Constraint_Text;

end Program.Nodes.Delta_Constraints;
