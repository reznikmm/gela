--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Enumeration_Literal_Specifications is

   function Create
    (Name : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access)
      return Enumeration_Literal_Specification is
   begin
      return Result : Enumeration_Literal_Specification :=
        (Name => Name, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Enumeration_Literal_Specification is
   begin
      return Result : Implicit_Enumeration_Literal_Specification :=
        (Name => Name, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Enumeration_Literal_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Enumeration_Literal_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Enumeration_Literal_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Enumeration_Literal_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Enumeration_Literal_Specification'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Enumeration_Literal_Specification_Element
    (Self : Base_Enumeration_Literal_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Enumeration_Literal_Specification_Element;

   overriding function Is_Declaration_Element
    (Self : Base_Enumeration_Literal_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration_Element;

   overriding procedure Visit
    (Self    : not null access Base_Enumeration_Literal_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Enumeration_Literal_Specification (Self);
   end Visit;

   overriding function To_Enumeration_Literal_Specification_Text
    (Self : aliased in out Enumeration_Literal_Specification)
      return Program.Elements.Enumeration_Literal_Specifications
          .Enumeration_Literal_Specification_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Enumeration_Literal_Specification_Text;

   overriding function To_Enumeration_Literal_Specification_Text
    (Self : aliased in out Implicit_Enumeration_Literal_Specification)
      return Program.Elements.Enumeration_Literal_Specifications
          .Enumeration_Literal_Specification_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Enumeration_Literal_Specification_Text;

end Program.Nodes.Enumeration_Literal_Specifications;
