--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Nodes.Package_Instantiations is

   function Create
    (Package_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Generic_Package_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters           : Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Right_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token           : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Package_Instantiation is
   begin
      return Result : Package_Instantiation :=
        (Package_Token => Package_Token, Name => Name, Is_Token => Is_Token,
         New_Token => New_Token, Generic_Package_Name => Generic_Package_Name,
         Left_Bracket_Token => Left_Bracket_Token, Parameters => Parameters,
         Right_Bracket_Token => Right_Bracket_Token, With_Token => With_Token,
         Aspects => Aspects, Semicolon_Token => Semicolon_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Defining_Names
         .Defining_Name_Access;
     Generic_Package_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Parameters           : Program.Elements.Parameter_Associations
         .Parameter_Association_Vector_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Package_Instantiation is
   begin
      return Result : Implicit_Package_Instantiation :=
        (Name => Name, Generic_Package_Name => Generic_Package_Name,
         Parameters => Parameters, Aspects => Aspects,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Package_Instantiation)
      return not null Program.Elements.Defining_Names.Defining_Name_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Generic_Package_Name
    (Self : Base_Package_Instantiation)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Generic_Package_Name;
   end Generic_Package_Name;

   overriding function Parameters
    (Self : Base_Package_Instantiation)
      return Program.Elements.Parameter_Associations
          .Parameter_Association_Vector_Access is
   begin
      return Self.Parameters;
   end Parameters;

   overriding function Aspects
    (Self : Base_Package_Instantiation)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is
   begin
      return Self.Aspects;
   end Aspects;

   overriding function Package_Token
    (Self : Package_Instantiation)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Package_Token;
   end Package_Token;

   overriding function Is_Token
    (Self : Package_Instantiation)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Is_Token;
   end Is_Token;

   overriding function New_Token
    (Self : Package_Instantiation)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.New_Token;
   end New_Token;

   overriding function Left_Bracket_Token
    (Self : Package_Instantiation)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Package_Instantiation)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function With_Token
    (Self : Package_Instantiation)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Semicolon_Token
    (Self : Package_Instantiation)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Package_Instantiation)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Package_Instantiation)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Package_Instantiation)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : in out Base_Package_Instantiation'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Generic_Package_Name, Self'Unchecked_Access);
      for Item in Self.Parameters.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Aspects.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Package_Instantiation
    (Self : Base_Package_Instantiation)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Package_Instantiation;

   overriding function Is_Declaration
    (Self : Base_Package_Instantiation)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Package_Instantiation;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Package_Instantiation (Self);
   end Visit;

   overriding function To_Package_Instantiation_Text
    (Self : in out Package_Instantiation)
      return Program.Elements.Package_Instantiations
          .Package_Instantiation_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Package_Instantiation_Text;

   overriding function To_Package_Instantiation_Text
    (Self : in out Implicit_Package_Instantiation)
      return Program.Elements.Package_Instantiations
          .Package_Instantiation_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Package_Instantiation_Text;

end Program.Nodes.Package_Instantiations;
