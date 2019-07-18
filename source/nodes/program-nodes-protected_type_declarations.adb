--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Protected_Type_Declarations is

   function Create
    (Protected_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Type_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name              : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Discriminant_Part : Program.Elements.Known_Discriminant_Parts
         .Known_Discriminant_Part_Access;
     With_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects           : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors       : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     With_Token_2      : Program.Lexical_Elements.Lexical_Element_Access;
     Definition        : not null Program.Elements.Protected_Definitions
         .Protected_Definition_Access;
     Semicolon_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Protected_Type_Declaration is
   begin
      return Result : Protected_Type_Declaration :=
        (Protected_Token => Protected_Token, Type_Token => Type_Token,
         Name => Name, Discriminant_Part => Discriminant_Part,
         With_Token => With_Token, Aspects => Aspects, Is_Token => Is_Token,
         New_Token => New_Token, Progenitors => Progenitors,
         With_Token_2 => With_Token_2, Definition => Definition,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Discriminant_Part    : Program.Elements.Known_Discriminant_Parts
         .Known_Discriminant_Part_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Progenitors          : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Definition           : not null Program.Elements.Protected_Definitions
         .Protected_Definition_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Protected_Type_Declaration is
   begin
      return Result : Implicit_Protected_Type_Declaration :=
        (Name => Name, Discriminant_Part => Discriminant_Part,
         Aspects => Aspects, Progenitors => Progenitors,
         Definition => Definition, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Protected_Type_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Discriminant_Part
    (Self : Base_Protected_Type_Declaration)
      return Program.Elements.Known_Discriminant_Parts
          .Known_Discriminant_Part_Access is
   begin
      return Self.Discriminant_Part;
   end Discriminant_Part;

   overriding function Aspects
    (Self : Base_Protected_Type_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is
   begin
      return Self.Aspects;
   end Aspects;

   overriding function Progenitors
    (Self : Base_Protected_Type_Declaration)
      return not null Program.Elements.Expressions.Expression_Vector_Access is
   begin
      return Self.Progenitors;
   end Progenitors;

   overriding function Definition
    (Self : Base_Protected_Type_Declaration)
      return not null Program.Elements.Protected_Definitions
          .Protected_Definition_Access is
   begin
      return Self.Definition;
   end Definition;

   overriding function Protected_Token
    (Self : Protected_Type_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Protected_Token;
   end Protected_Token;

   overriding function Type_Token
    (Self : Protected_Type_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Type_Token;
   end Type_Token;

   overriding function With_Token
    (Self : Protected_Type_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Is_Token
    (Self : Protected_Type_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Is_Token;
   end Is_Token;

   overriding function New_Token
    (Self : Protected_Type_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.New_Token;
   end New_Token;

   overriding function With_Token_2
    (Self : Protected_Type_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token_2;
   end With_Token_2;

   overriding function Semicolon_Token
    (Self : Protected_Type_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Protected_Type_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Protected_Type_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Protected_Type_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Protected_Type_Declaration'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      if Self.Discriminant_Part.Assigned then
         Set_Enclosing_Element (Self.Discriminant_Part, Self'Unchecked_Access);
      end if;
      for Item in Self.Aspects.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Progenitors.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      Set_Enclosing_Element (Self.Definition, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Protected_Type_Declaration
    (Self : Base_Protected_Type_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Protected_Type_Declaration;

   overriding function Is_Declaration
    (Self : Base_Protected_Type_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Protected_Type_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Protected_Type_Declaration (Self);
   end Visit;

   overriding function To_Protected_Type_Declaration_Text
    (Self : aliased in out Protected_Type_Declaration)
      return Program.Elements.Protected_Type_Declarations
          .Protected_Type_Declaration_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Protected_Type_Declaration_Text;

   overriding function To_Protected_Type_Declaration_Text
    (Self : aliased in out Implicit_Protected_Type_Declaration)
      return Program.Elements.Protected_Type_Declarations
          .Protected_Type_Declaration_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Protected_Type_Declaration_Text;

end Program.Nodes.Protected_Type_Declarations;
