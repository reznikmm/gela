--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Type_Declarations is

   function Create
    (Type_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name              : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Discriminant_Part : Program.Elements.Definitions.Definition_Access;
     Is_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Definition        : not null Program.Elements.Definitions
         .Definition_Access;
     With_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects           : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Type_Declaration is
   begin
      return Result : Type_Declaration :=
        (Type_Token => Type_Token, Name => Name,
         Discriminant_Part => Discriminant_Part, Is_Token => Is_Token,
         Definition => Definition, With_Token => With_Token,
         Aspects => Aspects, Semicolon_Token => Semicolon_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Discriminant_Part    : Program.Elements.Definitions.Definition_Access;
     Definition           : not null Program.Elements.Definitions
         .Definition_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Type_Declaration is
   begin
      return Result : Implicit_Type_Declaration :=
        (Name => Name, Discriminant_Part => Discriminant_Part,
         Definition => Definition, Aspects => Aspects,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Type_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Discriminant_Part
    (Self : Base_Type_Declaration)
      return Program.Elements.Definitions.Definition_Access is
   begin
      return Self.Discriminant_Part;
   end Discriminant_Part;

   overriding function Definition
    (Self : Base_Type_Declaration)
      return not null Program.Elements.Definitions.Definition_Access is
   begin
      return Self.Definition;
   end Definition;

   overriding function Aspects
    (Self : Base_Type_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is
   begin
      return Self.Aspects;
   end Aspects;

   overriding function Type_Token
    (Self : Type_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Type_Token;
   end Type_Token;

   overriding function Is_Token
    (Self : Type_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Is_Token;
   end Is_Token;

   overriding function With_Token
    (Self : Type_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Semicolon_Token
    (Self : Type_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Type_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Type_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Type_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Type_Declaration'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      if Self.Discriminant_Part.Assigned then
         Set_Enclosing_Element (Self.Discriminant_Part, Self'Unchecked_Access);
      end if;
      Set_Enclosing_Element (Self.Definition, Self'Unchecked_Access);
      for Item in Self.Aspects.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Type_Declaration
    (Self : Base_Type_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Type_Declaration;

   overriding function Is_Declaration
    (Self : Base_Type_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Type_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Type_Declaration (Self);
   end Visit;

   overriding function To_Type_Declaration_Text
    (Self : aliased in out Type_Declaration)
      return Program.Elements.Type_Declarations.Type_Declaration_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Type_Declaration_Text;

   overriding function To_Type_Declaration_Text
    (Self : aliased in out Implicit_Type_Declaration)
      return Program.Elements.Type_Declarations.Type_Declaration_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Type_Declaration_Text;

end Program.Nodes.Type_Declarations;
