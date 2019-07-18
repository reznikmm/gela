--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Package_Declarations is

   function Create
    (Package_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     With_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Visible_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     Private_Token        : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Name             : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Package_Declaration is
   begin
      return Result : Package_Declaration :=
        (Package_Token => Package_Token, Name => Name,
         With_Token => With_Token, Aspects => Aspects, Is_Token => Is_Token,
         Visible_Declarations => Visible_Declarations,
         Private_Token => Private_Token,
         Private_Declarations => Private_Declarations, End_Token => End_Token,
         End_Name => End_Name, Semicolon_Token => Semicolon_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Visible_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     Private_Declarations : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Name             : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Package_Declaration is
   begin
      return Result : Implicit_Package_Declaration :=
        (Name => Name, Aspects => Aspects,
         Visible_Declarations => Visible_Declarations,
         Private_Declarations => Private_Declarations, End_Name => End_Name,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Package_Declaration)
      return not null Program.Elements.Defining_Names.Defining_Name_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Aspects
    (Self : Base_Package_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is
   begin
      return Self.Aspects;
   end Aspects;

   overriding function Visible_Declarations
    (Self : Base_Package_Declaration)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Visible_Declarations;
   end Visible_Declarations;

   overriding function Private_Declarations
    (Self : Base_Package_Declaration)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Private_Declarations;
   end Private_Declarations;

   overriding function End_Name
    (Self : Base_Package_Declaration)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.End_Name;
   end End_Name;

   overriding function Package_Token
    (Self : Package_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Package_Token;
   end Package_Token;

   overriding function With_Token
    (Self : Package_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Is_Token
    (Self : Package_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Is_Token;
   end Is_Token;

   overriding function Private_Token
    (Self : Package_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Private_Token;
   end Private_Token;

   overriding function End_Token
    (Self : Package_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.End_Token;
   end End_Token;

   overriding function Semicolon_Token
    (Self : Package_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Package_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Package_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Package_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Package_Declaration'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      for Item in Self.Aspects.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Visible_Declarations.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Private_Declarations.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      if Self.End_Name.Assigned then
         Set_Enclosing_Element (Self.End_Name, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Package_Declaration
    (Self : Base_Package_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Package_Declaration;

   overriding function Is_Declaration
    (Self : Base_Package_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Package_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Package_Declaration (Self);
   end Visit;

   overriding function To_Package_Declaration_Text
    (Self : aliased in out Package_Declaration)
      return Program.Elements.Package_Declarations
          .Package_Declaration_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Package_Declaration_Text;

   overriding function To_Package_Declaration_Text
    (Self : aliased in out Implicit_Package_Declaration)
      return Program.Elements.Package_Declarations
          .Package_Declaration_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Package_Declaration_Text;

end Program.Nodes.Package_Declarations;
