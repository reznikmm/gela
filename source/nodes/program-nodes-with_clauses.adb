--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.With_Clauses is

   function Create
    (Limited_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Private_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     With_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Clause_Names    : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return With_Clause is
   begin
      return Result : With_Clause :=
        (Limited_Token => Limited_Token, Private_Token => Private_Token,
         With_Token => With_Token, Clause_Names => Clause_Names,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Clause_Names         : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_Limited          : Boolean := False;
     Has_Private          : Boolean := False)
      return Implicit_With_Clause is
   begin
      return Result : Implicit_With_Clause :=
        (Clause_Names => Clause_Names,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_Limited => Has_Limited, Has_Private => Has_Private,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Clause_Names
    (Self : Base_With_Clause)
      return not null Program.Elements.Expressions.Expression_Vector_Access is
   begin
      return Self.Clause_Names;
   end Clause_Names;

   overriding function Limited_Token
    (Self : With_Clause)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Limited_Token;
   end Limited_Token;

   overriding function Private_Token
    (Self : With_Clause)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Private_Token;
   end Private_Token;

   overriding function With_Token
    (Self : With_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Semicolon_Token
    (Self : With_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Has_Limited (Self : With_Clause) return Boolean is
   begin
      return Self.Limited_Token.Assigned;
   end Has_Limited;

   overriding function Has_Private (Self : With_Clause) return Boolean is
   begin
      return Self.Private_Token.Assigned;
   end Has_Private;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_With_Clause)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_With_Clause)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_With_Clause)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_Limited
    (Self : Implicit_With_Clause)
      return Boolean is
   begin
      return Self.Has_Limited;
   end Has_Limited;

   overriding function Has_Private
    (Self : Implicit_With_Clause)
      return Boolean is
   begin
      return Self.Has_Private;
   end Has_Private;

   procedure Initialize (Self : aliased in out Base_With_Clause'Class) is
   begin
      for Item in Self.Clause_Names.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_With_Clause
    (Self : Base_With_Clause)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_With_Clause;

   overriding function Is_Clause (Self : Base_With_Clause) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Clause;

   overriding procedure Visit
    (Self    : not null access Base_With_Clause;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.With_Clause (Self);
   end Visit;

   overriding function To_With_Clause_Text
    (Self : aliased in out With_Clause)
      return Program.Elements.With_Clauses.With_Clause_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_With_Clause_Text;

   overriding function To_With_Clause_Text
    (Self : aliased in out Implicit_With_Clause)
      return Program.Elements.With_Clauses.With_Clause_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_With_Clause_Text;

end Program.Nodes.With_Clauses;
