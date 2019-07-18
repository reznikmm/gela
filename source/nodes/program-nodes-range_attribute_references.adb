--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Range_Attribute_References is

   function Create
    (Range_Attribute : not null Program.Elements.Attribute_References
         .Attribute_Reference_Access)
      return Range_Attribute_Reference is
   begin
      return Result : Range_Attribute_Reference :=
        (Range_Attribute => Range_Attribute, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Range_Attribute      : not null Program.Elements.Attribute_References
         .Attribute_Reference_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Range_Attribute_Reference is
   begin
      return Result : Implicit_Range_Attribute_Reference :=
        (Range_Attribute => Range_Attribute,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Range_Attribute
    (Self : Base_Range_Attribute_Reference)
      return not null Program.Elements.Attribute_References
          .Attribute_Reference_Access is
   begin
      return Self.Range_Attribute;
   end Range_Attribute;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Range_Attribute_Reference)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Range_Attribute_Reference)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Range_Attribute_Reference)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Range_Attribute_Reference'Class) is
   begin
      Set_Enclosing_Element (Self.Range_Attribute, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Range_Attribute_Reference
    (Self : Base_Range_Attribute_Reference)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Range_Attribute_Reference;

   overriding function Is_Constraint
    (Self : Base_Range_Attribute_Reference)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Constraint;

   overriding function Is_Definition
    (Self : Base_Range_Attribute_Reference)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition;

   overriding function Is_Discrete_Subtype_Definition
    (Self : Base_Range_Attribute_Reference)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Discrete_Subtype_Definition;

   overriding function Is_Discrete_Range
    (Self : Base_Range_Attribute_Reference)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Discrete_Range;

   overriding procedure Visit
    (Self    : not null access Base_Range_Attribute_Reference;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Range_Attribute_Reference (Self);
   end Visit;

   overriding function To_Range_Attribute_Reference_Text
    (Self : aliased in out Range_Attribute_Reference)
      return Program.Elements.Range_Attribute_References
          .Range_Attribute_Reference_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Range_Attribute_Reference_Text;

   overriding function To_Range_Attribute_Reference_Text
    (Self : aliased in out Implicit_Range_Attribute_Reference)
      return Program.Elements.Range_Attribute_References
          .Range_Attribute_Reference_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Range_Attribute_Reference_Text;

end Program.Nodes.Range_Attribute_References;
