--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Number_Declarations is

   function Create
    (Names            : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Colon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Constant_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Assignment_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression       : not null Program.Elements.Expressions
         .Expression_Access;
     Semicolon_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Number_Declaration is
   begin
      return Result : Number_Declaration :=
        (Names => Names, Colon_Token => Colon_Token,
         Constant_Token => Constant_Token,
         Assignment_Token => Assignment_Token, Expression => Expression,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Names                : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Vector_Access;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Number_Declaration is
   begin
      return Result : Implicit_Number_Declaration :=
        (Names => Names, Expression => Expression,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Names
    (Self : Base_Number_Declaration)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Vector_Access is
   begin
      return Self.Names;
   end Names;

   overriding function Expression
    (Self : Base_Number_Declaration)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Expression;
   end Expression;

   overriding function Colon_Token
    (Self : Number_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Colon_Token;
   end Colon_Token;

   overriding function Constant_Token
    (Self : Number_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Constant_Token;
   end Constant_Token;

   overriding function Assignment_Token
    (Self : Number_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Assignment_Token;
   end Assignment_Token;

   overriding function Semicolon_Token
    (Self : Number_Declaration)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Number_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Number_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Number_Declaration)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Number_Declaration'Class) is
   begin
      for Item in Self.Names.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      Set_Enclosing_Element (Self.Expression, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Number_Declaration_Element
    (Self : Base_Number_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Number_Declaration_Element;

   overriding function Is_Declaration_Element
    (Self : Base_Number_Declaration)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Declaration_Element;

   overriding procedure Visit
    (Self    : not null access Base_Number_Declaration;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Number_Declaration (Self);
   end Visit;

   overriding function To_Number_Declaration_Text
    (Self : aliased in out Number_Declaration)
      return Program.Elements.Number_Declarations
          .Number_Declaration_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Number_Declaration_Text;

   overriding function To_Number_Declaration_Text
    (Self : aliased in out Implicit_Number_Declaration)
      return Program.Elements.Number_Declarations
          .Number_Declaration_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Number_Declaration_Text;

end Program.Nodes.Number_Declarations;
