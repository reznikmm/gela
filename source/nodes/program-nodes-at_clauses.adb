--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.At_Clauses is

   function Create
    (For_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Identifiers.Identifier_Access;
     Use_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     At_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression      : not null Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return At_Clause is
   begin
      return Result : At_Clause :=
        (For_Token => For_Token, Name => Name, Use_Token => Use_Token,
         At_Token => At_Token, Expression => Expression,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Identifiers
         .Identifier_Access;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_At_Clause is
   begin
      return Result : Implicit_At_Clause :=
        (Name => Name, Expression => Expression,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_At_Clause)
      return not null Program.Elements.Identifiers.Identifier_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Expression
    (Self : Base_At_Clause)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Expression;
   end Expression;

   overriding function For_Token
    (Self : At_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.For_Token;
   end For_Token;

   overriding function Use_Token
    (Self : At_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Use_Token;
   end Use_Token;

   overriding function At_Token
    (Self : At_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.At_Token;
   end At_Token;

   overriding function Semicolon_Token
    (Self : At_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_At_Clause)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_At_Clause)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_At_Clause)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_At_Clause'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Expression, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_At_Clause_Element
    (Self : Base_At_Clause)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_At_Clause_Element;

   overriding function Is_Representation_Clause_Element
    (Self : Base_At_Clause)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Representation_Clause_Element;

   overriding function Is_Clause_Element
    (Self : Base_At_Clause)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Clause_Element;

   overriding procedure Visit
    (Self    : not null access Base_At_Clause;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.At_Clause (Self);
   end Visit;

   overriding function To_At_Clause_Text
    (Self : aliased in out At_Clause)
      return Program.Elements.At_Clauses.At_Clause_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_At_Clause_Text;

   overriding function To_At_Clause_Text
    (Self : aliased in out Implicit_At_Clause)
      return Program.Elements.At_Clauses.At_Clause_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_At_Clause_Text;

end Program.Nodes.At_Clauses;
