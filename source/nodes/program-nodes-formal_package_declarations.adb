--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Formal_Package_Declarations is

   function Create
    (With_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Package_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Generic_Package_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Left_Bracket_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Parameters           : not null Program.Elements
         .Formal_Package_Associations.Formal_Package_Association_Vector_Access;
     Right_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token_2         : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Formal_Package_Declaration is
   begin
      return Result : Formal_Package_Declaration :=
        (With_Token => With_Token, Package_Token => Package_Token,
         Name => Name, Is_Token => Is_Token, New_Token => New_Token,
         Generic_Package_Name => Generic_Package_Name,
         Left_Bracket_Token => Left_Bracket_Token, Parameters => Parameters,
         Right_Bracket_Token => Right_Bracket_Token,
         With_Token_2 => With_Token_2, Aspects => Aspects,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Generic_Package_Name : not null Program.Elements.Expressions
         .Expression_Access;
     Parameters           : not null Program.Elements
         .Formal_Package_Associations.Formal_Package_Association_Vector_Access;
     Aspects              : not null Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Formal_Package_Declaration is
   begin
      return Result : Implicit_Formal_Package_Declaration :=
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
    (Self : Base_Formal_Package_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Generic_Package_Name
    (Self : Base_Formal_Package_Declaration)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Generic_Package_Name;
   end Generic_Package_Name;

   overriding function Parameters
    (Self : Base_Formal_Package_Declaration)
      return not null Program.Elements.Formal_Package_Associations
          .Formal_Package_Association_Vector_Access is
   begin
      return Self.Parameters;
   end Parameters;

   overriding function Aspects
    (Self : Base_Formal_Package_Declaration)
      return not null Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is
   begin
      return Self.Aspects;
   end Aspects;

   overriding function With_Token
    (Self : Formal_Package_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Package_Token
    (Self : Formal_Package_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Package_Token;
   end Package_Token;

   overriding function Is_Token
    (Self : Formal_Package_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Is_Token;
   end Is_Token;

   overriding function New_Token
    (Self : Formal_Package_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.New_Token;
   end New_Token;

   overriding function Left_Bracket_Token
    (Self : Formal_Package_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Formal_Package_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function With_Token_2
    (Self : Formal_Package_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token_2;
   end With_Token_2;

   overriding function Semicolon_Token
    (Self : Formal_Package_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Formal_Package_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Formal_Package_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Formal_Package_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Formal_Package_Declaration'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Generic_Package_Name, Self'Unchecked_Access);
      for Item in Self.Parameters.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Aspects.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Formal_Package_Declaration
    (Self : Base_Formal_Package_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Formal_Package_Declaration;

   overriding function Is_Declaration
    (Self : Base_Formal_Package_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Formal_Package_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Formal_Package_Declaration (Self);
   end Visit;

   overriding function To_Formal_Package_Declaration_Text
    (Self : aliased in out Formal_Package_Declaration)
      return Program.Elements.Formal_Package_Declarations
          .Formal_Package_Declaration_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Formal_Package_Declaration_Text;

   overriding function To_Formal_Package_Declaration_Text
    (Self : aliased in out Implicit_Formal_Package_Declaration)
      return Program.Elements.Formal_Package_Declarations
          .Formal_Package_Declaration_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Formal_Package_Declaration_Text;

end Program.Nodes.Formal_Package_Declarations;
