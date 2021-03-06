--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Nodes.Single_Task_Declarations is

   function Create
    (Task_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Aspects         : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Is_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     New_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Progenitors     : Program.Elements.Expressions.Expression_Vector_Access;
     With_Token_2    : Program.Lexical_Elements.Lexical_Element_Access;
     Definition      : not null Program.Elements.Task_Definitions
         .Task_Definition_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Single_Task_Declaration is
   begin
      return Result : Single_Task_Declaration :=
        (Task_Token => Task_Token, Name => Name, With_Token => With_Token,
         Aspects => Aspects, Is_Token => Is_Token, New_Token => New_Token,
         Progenitors => Progenitors, With_Token_2 => With_Token_2,
         Definition => Definition, Semicolon_Token => Semicolon_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Aspects              : Program.Elements.Aspect_Specifications
         .Aspect_Specification_Vector_Access;
     Progenitors          : Program.Elements.Expressions
         .Expression_Vector_Access;
     Definition           : not null Program.Elements.Task_Definitions
         .Task_Definition_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Single_Task_Declaration is
   begin
      return Result : Implicit_Single_Task_Declaration :=
        (Name => Name, Aspects => Aspects, Progenitors => Progenitors,
         Definition => Definition, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Single_Task_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Aspects
    (Self : Base_Single_Task_Declaration)
      return Program.Elements.Aspect_Specifications
          .Aspect_Specification_Vector_Access is
   begin
      return Self.Aspects;
   end Aspects;

   overriding function Progenitors
    (Self : Base_Single_Task_Declaration)
      return Program.Elements.Expressions.Expression_Vector_Access is
   begin
      return Self.Progenitors;
   end Progenitors;

   overriding function Definition
    (Self : Base_Single_Task_Declaration)
      return not null Program.Elements.Task_Definitions
          .Task_Definition_Access is
   begin
      return Self.Definition;
   end Definition;

   overriding function Task_Token
    (Self : Single_Task_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Task_Token;
   end Task_Token;

   overriding function With_Token
    (Self : Single_Task_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Is_Token
    (Self : Single_Task_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Is_Token;
   end Is_Token;

   overriding function New_Token
    (Self : Single_Task_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.New_Token;
   end New_Token;

   overriding function With_Token_2
    (Self : Single_Task_Declaration)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token_2;
   end With_Token_2;

   overriding function Semicolon_Token
    (Self : Single_Task_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Single_Task_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Single_Task_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Single_Task_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : in out Base_Single_Task_Declaration'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      for Item in Self.Aspects.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Progenitors.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      Set_Enclosing_Element (Self.Definition, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Single_Task_Declaration
    (Self : Base_Single_Task_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Single_Task_Declaration;

   overriding function Is_Declaration
    (Self : Base_Single_Task_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration;

   overriding procedure Visit
    (Self    : not null access Base_Single_Task_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Single_Task_Declaration (Self);
   end Visit;

   overriding function To_Single_Task_Declaration_Text
    (Self : in out Single_Task_Declaration)
      return Program.Elements.Single_Task_Declarations
          .Single_Task_Declaration_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Single_Task_Declaration_Text;

   overriding function To_Single_Task_Declaration_Text
    (Self : in out Implicit_Single_Task_Declaration)
      return Program.Elements.Single_Task_Declarations
          .Single_Task_Declaration_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Single_Task_Declaration_Text;

end Program.Nodes.Single_Task_Declarations;
