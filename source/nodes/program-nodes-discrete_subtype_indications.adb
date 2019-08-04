--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Discrete_Subtype_Indications is

   function Create
    (Subtype_Mark : not null Program.Elements.Expressions.Expression_Access;
     Constraint   : Program.Elements.Constraints.Constraint_Access)
      return Discrete_Subtype_Indication is
   begin
      return Result : Discrete_Subtype_Indication :=
        (Subtype_Mark => Subtype_Mark, Constraint => Constraint,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Subtype_Mark         : not null Program.Elements.Expressions
         .Expression_Access;
     Constraint           : Program.Elements.Constraints.Constraint_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Discrete_Subtype_Indication is
   begin
      return Result : Implicit_Discrete_Subtype_Indication :=
        (Subtype_Mark => Subtype_Mark, Constraint => Constraint,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Subtype_Mark
    (Self : Base_Discrete_Subtype_Indication)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Subtype_Mark;
   end Subtype_Mark;

   overriding function Constraint
    (Self : Base_Discrete_Subtype_Indication)
      return Program.Elements.Constraints.Constraint_Access is
   begin
      return Self.Constraint;
   end Constraint;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Discrete_Subtype_Indication)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Discrete_Subtype_Indication)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Discrete_Subtype_Indication)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Discrete_Subtype_Indication'Class) is
   begin
      Set_Enclosing_Element (Self.Subtype_Mark, Self'Unchecked_Access);
      if Self.Constraint.Assigned then
         Set_Enclosing_Element (Self.Constraint, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Discrete_Subtype_Indication
    (Self : Base_Discrete_Subtype_Indication)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Discrete_Subtype_Indication;

   overriding function Is_Discrete_Range
    (Self : Base_Discrete_Subtype_Indication)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Discrete_Range;

   overriding function Is_Definition
    (Self : Base_Discrete_Subtype_Indication)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding procedure Visit
    (Self    : not null access Base_Discrete_Subtype_Indication;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Discrete_Subtype_Indication (Self);
   end Visit;

   overriding function To_Discrete_Subtype_Indication_Text
    (Self : aliased in out Discrete_Subtype_Indication)
      return Program.Elements.Discrete_Subtype_Indications
          .Discrete_Subtype_Indication_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Discrete_Subtype_Indication_Text;

   overriding function To_Discrete_Subtype_Indication_Text
    (Self : aliased in out Implicit_Discrete_Subtype_Indication)
      return Program.Elements.Discrete_Subtype_Indications
          .Discrete_Subtype_Indication_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Discrete_Subtype_Indication_Text;

end Program.Nodes.Discrete_Subtype_Indications;
