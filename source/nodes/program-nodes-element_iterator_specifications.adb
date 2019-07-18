--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Element_Iterator_Specifications is

   function Create
    (Name               : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Subtype_Indication : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Of_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Reverse_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Iterable_Name      : not null Program.Elements.Expressions
         .Expression_Access)
      return Element_Iterator_Specification is
   begin
      return Result : Element_Iterator_Specification :=
        (Name => Name, Colon_Token => Colon_Token,
         Subtype_Indication => Subtype_Indication, Of_Token => Of_Token,
         Reverse_Token => Reverse_Token, Iterable_Name => Iterable_Name,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Subtype_Indication   : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     Iterable_Name        : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Reverse          : Boolean := False)
      return Implicit_Element_Iterator_Specification is
   begin
      return Result : Implicit_Element_Iterator_Specification :=
        (Name => Name, Subtype_Indication => Subtype_Indication,
         Iterable_Name => Iterable_Name,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Reverse => Has_Reverse, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Element_Iterator_Specification)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Subtype_Indication
    (Self : Base_Element_Iterator_Specification)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access is
   begin
      return Self.Subtype_Indication;
   end Subtype_Indication;

   overriding function Iterable_Name
    (Self : Base_Element_Iterator_Specification)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Iterable_Name;
   end Iterable_Name;

   overriding function Colon_Token
    (Self : Element_Iterator_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Colon_Token;
   end Colon_Token;

   overriding function Of_Token
    (Self : Element_Iterator_Specification)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Of_Token;
   end Of_Token;

   overriding function Reverse_Token
    (Self : Element_Iterator_Specification)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Reverse_Token;
   end Reverse_Token;

   overriding function Has_Reverse
    (Self : Element_Iterator_Specification)
      return Boolean is
   begin
      return Self.Reverse_Token.Assigned;
   end Has_Reverse;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Element_Iterator_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Element_Iterator_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Element_Iterator_Specification)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Reverse
    (Self : Implicit_Element_Iterator_Specification)
      return Boolean is
   begin
      return Self.Has_Reverse;
   end Has_Reverse;

   procedure Initialize
    (Self : aliased in out Base_Element_Iterator_Specification'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Subtype_Indication, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Iterable_Name, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Element_Iterator_Specification
    (Self : Base_Element_Iterator_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Element_Iterator_Specification;

   overriding function Is_Declaration
    (Self : Base_Element_Iterator_Specification)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Element_Iterator_Specification;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Element_Iterator_Specification (Self);
   end Visit;

   overriding function To_Element_Iterator_Specification_Text
    (Self : aliased in out Element_Iterator_Specification)
      return Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Element_Iterator_Specification_Text;

   overriding function To_Element_Iterator_Specification_Text
    (Self : aliased in out Implicit_Element_Iterator_Specification)
      return Program.Elements.Element_Iterator_Specifications
          .Element_Iterator_Specification_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Element_Iterator_Specification_Text;

end Program.Nodes.Element_Iterator_Specifications;
