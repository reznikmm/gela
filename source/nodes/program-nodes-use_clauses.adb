--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Use_Clauses is

   function Create
    (Use_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     All_Token       : Program.Lexical_Elements.Lexical_Element_Access;
     Type_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Clause_Names    : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Use_Clause is
   begin
      return Result : Use_Clause :=
        (Use_Token => Use_Token, All_Token => All_Token,
         Type_Token => Type_Token, Clause_Names => Clause_Names,
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
     Has_All              : Boolean := False;
     Has_Type             : Boolean := False)
      return Implicit_Use_Clause is
   begin
      return Result : Implicit_Use_Clause :=
        (Clause_Names => Clause_Names,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Has_All => Has_All,
         Has_Type => Has_Type, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Clause_Names
    (Self : Base_Use_Clause)
      return not null Program.Elements.Expressions.Expression_Vector_Access is
   begin
      return Self.Clause_Names;
   end Clause_Names;

   overriding function Use_Token
    (Self : Use_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Use_Token;
   end Use_Token;

   overriding function All_Token
    (Self : Use_Clause)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.All_Token;
   end All_Token;

   overriding function Type_Token
    (Self : Use_Clause)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Type_Token;
   end Type_Token;

   overriding function Semicolon_Token
    (Self : Use_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Has_All (Self : Use_Clause) return Boolean is
   begin
      return Self.All_Token.Assigned;
   end Has_All;

   overriding function Has_Type (Self : Use_Clause) return Boolean is
   begin
      return Self.Type_Token.Assigned;
   end Has_Type;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Use_Clause)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Use_Clause)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Use_Clause)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_All (Self : Implicit_Use_Clause) return Boolean is
   begin
      return Self.Has_All;
   end Has_All;

   overriding function Has_Type (Self : Implicit_Use_Clause) return Boolean is
   begin
      return Self.Has_Type;
   end Has_Type;

   procedure Initialize (Self : aliased in out Base_Use_Clause'Class) is
   begin
      for Item in Self.Clause_Names.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Use_Clause (Self : Base_Use_Clause) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Use_Clause;

   overriding function Is_Clause (Self : Base_Use_Clause) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Clause;

   overriding procedure Visit
    (Self    : not null access Base_Use_Clause;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Use_Clause (Self);
   end Visit;

   overriding function To_Use_Clause_Text
    (Self : aliased in out Use_Clause)
      return Program.Elements.Use_Clauses.Use_Clause_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Use_Clause_Text;

   overriding function To_Use_Clause_Text
    (Self : aliased in out Implicit_Use_Clause)
      return Program.Elements.Use_Clauses.Use_Clause_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Use_Clause_Text;

end Program.Nodes.Use_Clauses;
