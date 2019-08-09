--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Generalized_Iterator_Specifications is

   function Create
    (Name          : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     In_Token      : not null Program.Lexical_Elements.Lexical_Element_Access;
     Reverse_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Iterator_Name : not null Program.Elements.Expressions.Expression_Access)
      return Generalized_Iterator_Specification is
   begin
      return Result : Generalized_Iterator_Specification :=
        (Name => Name, In_Token => In_Token, Reverse_Token => Reverse_Token,
         Iterator_Name => Iterator_Name, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Iterator_Name        : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Reverse          : Boolean := False)
      return Implicit_Generalized_Iterator_Specification is
   begin
      return Result : Implicit_Generalized_Iterator_Specification :=
        (Name => Name, Iterator_Name => Iterator_Name,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Reverse => Has_Reverse, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Generalized_Iterator_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Iterator_Name
    (Self : Base_Generalized_Iterator_Specification)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Iterator_Name;
   end Iterator_Name;

   overriding function In_Token
    (Self : Generalized_Iterator_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.In_Token;
   end In_Token;

   overriding function Reverse_Token
    (Self : Generalized_Iterator_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Reverse_Token;
   end Reverse_Token;

   overriding function Has_Reverse
    (Self : Generalized_Iterator_Specification)
      return Boolean is
   begin
      return Self.Reverse_Token.Assigned;
   end Has_Reverse;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Generalized_Iterator_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Generalized_Iterator_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Generalized_Iterator_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Reverse
    (Self : Implicit_Generalized_Iterator_Specification)
      return Boolean is
   begin
      return Self.Has_Reverse;
   end Has_Reverse;

   procedure Initialize
    (Self : aliased in out Base_Generalized_Iterator_Specification'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Iterator_Name, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Generalized_Iterator_Specification_Element
    (Self : Base_Generalized_Iterator_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Generalized_Iterator_Specification_Element;

   overriding function Is_Declaration_Element
    (Self : Base_Generalized_Iterator_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration_Element;

   overriding procedure Visit
    (Self    : not null access Base_Generalized_Iterator_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Generalized_Iterator_Specification (Self);
   end Visit;

   overriding function To_Generalized_Iterator_Specification_Text
    (Self : aliased in out Generalized_Iterator_Specification)
      return Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Generalized_Iterator_Specification_Text;

   overriding function To_Generalized_Iterator_Specification_Text
    (Self : aliased in out Implicit_Generalized_Iterator_Specification)
      return Program.Elements.Generalized_Iterator_Specifications
          .Generalized_Iterator_Specification_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Generalized_Iterator_Specification_Text;

end Program.Nodes.Generalized_Iterator_Specifications;
