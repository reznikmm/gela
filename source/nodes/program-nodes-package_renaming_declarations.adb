--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Package_Renaming_Declarations is

   function Create
    (Package_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Renames_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Renamed_Package : not null Program.Elements.Expressions.Expression_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Package_Renaming_Declaration is
   begin
      return Result : Package_Renaming_Declaration :=
        (Package_Token => Package_Token, Name => Name,
         Renames_Token => Renames_Token, Renamed_Package => Renamed_Package,
         With_Token => With_Token, Aspects => Aspects,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Renamed_Package      : not null Program.Elements.Expressions
         .Expression_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Package_Renaming_Declaration is
   begin
      return Result : Implicit_Package_Renaming_Declaration :=
        (Name => Name, Renamed_Package => Renamed_Package, Aspects => Aspects,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Package_Renaming_Declaration)
      return not null Program.Elements.Defining_Names.Defining_Name_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Renamed_Package
    (Self : Base_Package_Renaming_Declaration)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Renamed_Package;
   end Renamed_Package;

   overriding function Aspects
    (Self : Base_Package_Renaming_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is
   begin
      return Self.Aspects;
   end Aspects;

   overriding function Package_Token
    (Self : Package_Renaming_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Package_Token;
   end Package_Token;

   overriding function Renames_Token
    (Self : Package_Renaming_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Renames_Token;
   end Renames_Token;

   overriding function With_Token
    (Self : Package_Renaming_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Semicolon_Token
    (Self : Package_Renaming_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Package_Renaming_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Package_Renaming_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Package_Renaming_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Package_Renaming_Declaration'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Renamed_Package, Self'Unchecked_Access);
      for Item in Self.Aspects.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Package_Renaming_Declaration
    (Self : Base_Package_Renaming_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Package_Renaming_Declaration;

   overriding function Is_Declaration
    (Self : Base_Package_Renaming_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Package_Renaming_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Package_Renaming_Declaration (Self);
   end Visit;

   overriding function To_Package_Renaming_Declaration_Text
    (Self : aliased in out Package_Renaming_Declaration)
      return Program.Elements.Package_Renaming_Declarations
          .Package_Renaming_Declaration_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Package_Renaming_Declaration_Text;

   overriding function To_Package_Renaming_Declaration_Text
    (Self : aliased in out Implicit_Package_Renaming_Declaration)
      return Program.Elements.Package_Renaming_Declarations
          .Package_Renaming_Declaration_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Package_Renaming_Declaration_Text;

end Program.Nodes.Package_Renaming_Declarations;
