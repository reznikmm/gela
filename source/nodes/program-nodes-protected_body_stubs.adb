--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Protected_Body_Stubs is

   function Create
    (Protected_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Body_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Separate_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Protected_Body_Stub is
   begin
      return Result : Protected_Body_Stub :=
        (Protected_Token => Protected_Token, Body_Token => Body_Token,
         Name => Name, Is_Token => Is_Token, Separate_Token => Separate_Token,
         With_Token => With_Token, Aspects => Aspects,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Protected_Body_Stub is
   begin
      return Result : Implicit_Protected_Body_Stub :=
        (Name => Name, Aspects => Aspects,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Protected_Body_Stub)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Aspects
    (Self : Base_Protected_Body_Stub)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is
   begin
      return Self.Aspects;
   end Aspects;

   overriding function Protected_Token
    (Self : Protected_Body_Stub)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Protected_Token;
   end Protected_Token;

   overriding function Body_Token
    (Self : Protected_Body_Stub)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Body_Token;
   end Body_Token;

   overriding function Is_Token
    (Self : Protected_Body_Stub)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Is_Token;
   end Is_Token;

   overriding function Separate_Token
    (Self : Protected_Body_Stub)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Separate_Token;
   end Separate_Token;

   overriding function With_Token
    (Self : Protected_Body_Stub)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Semicolon_Token
    (Self : Protected_Body_Stub)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Protected_Body_Stub)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Protected_Body_Stub)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Protected_Body_Stub)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Protected_Body_Stub'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      for Item in Self.Aspects.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Protected_Body_Stub
    (Self : Base_Protected_Body_Stub)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Protected_Body_Stub;

   overriding function Is_Declaration
    (Self : Base_Protected_Body_Stub)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Protected_Body_Stub;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Protected_Body_Stub (Self);
   end Visit;

   overriding function To_Protected_Body_Stub_Text
    (Self : aliased in out Protected_Body_Stub)
      return Program.Elements.Protected_Body_Stubs
          .Protected_Body_Stub_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Protected_Body_Stub_Text;

   overriding function To_Protected_Body_Stub_Text
    (Self : aliased in out Implicit_Protected_Body_Stub)
      return Program.Elements.Protected_Body_Stubs
          .Protected_Body_Stub_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Protected_Body_Stub_Text;

end Program.Nodes.Protected_Body_Stubs;
