--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Discrete_Range_Attribute_References is

   function Create
    (Range_Attribute                : not null Program.Elements
         .Attribute_References.Attribute_Reference_Access;
     Is_Discrete_Subtype_Definition : Boolean := False)
      return Discrete_Range_Attribute_Reference is
   begin
      return Result : Discrete_Range_Attribute_Reference :=
        (Range_Attribute => Range_Attribute,
         Is_Discrete_Subtype_Definition => Is_Discrete_Subtype_Definition,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Range_Attribute                : not null Program.Elements
         .Attribute_References.Attribute_Reference_Access;
     Is_Part_Of_Implicit            : Boolean := False;
     Is_Part_Of_Inherited           : Boolean := False;
     Is_Part_Of_Instance            : Boolean := False;
     Is_Discrete_Subtype_Definition : Boolean := False)
      return Implicit_Discrete_Range_Attribute_Reference is
   begin
      return Result : Implicit_Discrete_Range_Attribute_Reference :=
        (Range_Attribute => Range_Attribute,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Is_Discrete_Subtype_Definition => Is_Discrete_Subtype_Definition,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Range_Attribute
    (Self : Base_Discrete_Range_Attribute_Reference)
      return not null Program.Elements.Attribute_References
          .Attribute_Reference_Access is
   begin
      return Self.Range_Attribute;
   end Range_Attribute;

   overriding function Is_Discrete_Subtype_Definition
    (Self : Base_Discrete_Range_Attribute_Reference)
      return Boolean is
   begin
      return Self.Is_Discrete_Subtype_Definition;
   end Is_Discrete_Subtype_Definition;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Discrete_Range_Attribute_Reference)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Discrete_Range_Attribute_Reference)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Discrete_Range_Attribute_Reference)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Discrete_Range_Attribute_Reference'Class) is
   begin
      Set_Enclosing_Element (Self.Range_Attribute, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Discrete_Range_Attribute_Reference_Element
    (Self : Base_Discrete_Range_Attribute_Reference)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Discrete_Range_Attribute_Reference_Element;

   overriding function Is_Discrete_Range_Element
    (Self : Base_Discrete_Range_Attribute_Reference)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Discrete_Range_Element;

   overriding function Is_Definition_Element
    (Self : Base_Discrete_Range_Attribute_Reference)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Definition_Element;

   overriding procedure Visit
    (Self    : not null access Base_Discrete_Range_Attribute_Reference;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Discrete_Range_Attribute_Reference (Self);
   end Visit;

   overriding function To_Discrete_Range_Attribute_Reference_Text
    (Self : aliased in out Discrete_Range_Attribute_Reference)
      return Program.Elements.Discrete_Range_Attribute_References
          .Discrete_Range_Attribute_Reference_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Discrete_Range_Attribute_Reference_Text;

   overriding function To_Discrete_Range_Attribute_Reference_Text
    (Self : aliased in out Implicit_Discrete_Range_Attribute_Reference)
      return Program.Elements.Discrete_Range_Attribute_References
          .Discrete_Range_Attribute_Reference_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Discrete_Range_Attribute_Reference_Text;

end Program.Nodes.Discrete_Range_Attribute_References;
