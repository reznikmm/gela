--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Subtype_Declarations is

   function Create
    (Subtype_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name               : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Subtype_Indication : not null Program.Elements.Subtype_Indications
         .Subtype_Indication_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects            : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Subtype_Declaration is
   begin
      return Result : Subtype_Declaration :=
        (Subtype_Token => Subtype_Token, Name => Name, Is_Token => Is_Token,
         Subtype_Indication => Subtype_Indication, With_Token => With_Token,
         Aspects => Aspects, Semicolon_Token => Semicolon_Token,
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
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Subtype_Declaration is
   begin
      return Result : Implicit_Subtype_Declaration :=
        (Name => Name, Subtype_Indication => Subtype_Indication,
         Aspects => Aspects, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Subtype_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Subtype_Indication
    (Self : Base_Subtype_Declaration)
      return not null Program.Elements.Subtype_Indications
          .Subtype_Indication_Access is
   begin
      return Self.Subtype_Indication;
   end Subtype_Indication;

   overriding function Aspects
    (Self : Base_Subtype_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is
   begin
      return Self.Aspects;
   end Aspects;

   overriding function Subtype_Token
    (Self : Subtype_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Subtype_Token;
   end Subtype_Token;

   overriding function Is_Token
    (Self : Subtype_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Is_Token;
   end Is_Token;

   overriding function With_Token
    (Self : Subtype_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Semicolon_Token
    (Self : Subtype_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Subtype_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Subtype_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Subtype_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Subtype_Declaration'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Subtype_Indication, Self'Unchecked_Access);
      for Item in Self.Aspects.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Subtype_Declaration
    (Self : Base_Subtype_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Subtype_Declaration;

   overriding function Is_Declaration
    (Self : Base_Subtype_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Subtype_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Subtype_Declaration (Self);
   end Visit;

   overriding function To_Subtype_Declaration_Text
    (Self : aliased in out Subtype_Declaration)
      return Program.Elements.Subtype_Declarations
          .Subtype_Declaration_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Subtype_Declaration_Text;

   overriding function To_Subtype_Declaration_Text
    (Self : aliased in out Implicit_Subtype_Declaration)
      return Program.Elements.Subtype_Declarations
          .Subtype_Declaration_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Subtype_Declaration_Text;

end Program.Nodes.Subtype_Declarations;
