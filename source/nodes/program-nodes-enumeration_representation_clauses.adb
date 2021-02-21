--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Nodes.Enumeration_Representation_Clauses is

   function Create
    (For_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name            : not null Program.Elements.Expressions.Expression_Access;
     Use_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression      : not null Program.Elements.Array_Aggregates
         .Array_Aggregate_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Enumeration_Representation_Clause is
   begin
      return Result : Enumeration_Representation_Clause :=
        (For_Token => For_Token, Name => Name, Use_Token => Use_Token,
         Expression => Expression, Semicolon_Token => Semicolon_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                 : not null Program.Elements.Expressions
         .Expression_Access;
     Expression           : not null Program.Elements.Array_Aggregates
         .Array_Aggregate_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Enumeration_Representation_Clause is
   begin
      return Result : Implicit_Enumeration_Representation_Clause :=
        (Name => Name, Expression => Expression,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Enumeration_Representation_Clause)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Expression
    (Self : Base_Enumeration_Representation_Clause)
      return not null Program.Elements.Array_Aggregates
          .Array_Aggregate_Access is
   begin
      return Self.Expression;
   end Expression;

   overriding function For_Token
    (Self : Enumeration_Representation_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.For_Token;
   end For_Token;

   overriding function Use_Token
    (Self : Enumeration_Representation_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Use_Token;
   end Use_Token;

   overriding function Semicolon_Token
    (Self : Enumeration_Representation_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Enumeration_Representation_Clause)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Enumeration_Representation_Clause)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Enumeration_Representation_Clause)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : in out Base_Enumeration_Representation_Clause'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Expression, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Enumeration_Representation_Clause
    (Self : Base_Enumeration_Representation_Clause)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Enumeration_Representation_Clause;

   overriding function Is_Representation_Clause
    (Self : Base_Enumeration_Representation_Clause)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Representation_Clause;

   overriding function Is_Clause
    (Self : Base_Enumeration_Representation_Clause)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Clause;

   overriding procedure Visit
    (Self    : not null access Base_Enumeration_Representation_Clause;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Enumeration_Representation_Clause (Self);
   end Visit;

   overriding function To_Enumeration_Representation_Clause_Text
    (Self : in out Enumeration_Representation_Clause)
      return Program.Elements.Enumeration_Representation_Clauses
          .Enumeration_Representation_Clause_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Enumeration_Representation_Clause_Text;

   overriding function To_Enumeration_Representation_Clause_Text
    (Self : in out Implicit_Enumeration_Representation_Clause)
      return Program.Elements.Enumeration_Representation_Clauses
          .Enumeration_Representation_Clause_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Enumeration_Representation_Clause_Text;

end Program.Nodes.Enumeration_Representation_Clauses;
