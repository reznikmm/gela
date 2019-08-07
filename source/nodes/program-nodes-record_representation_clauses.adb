--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Record_Representation_Clauses is

   function Create
    (For_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Name                  : not null Program.Elements.Expressions
         .Expression_Access;
     Use_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Record_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     At_Token              : Program.Lexical_Elements.Lexical_Element_Access;
     Mod_Token             : Program.Lexical_Elements.Lexical_Element_Access;
     Mod_Clause_Expression : Program.Elements.Expressions.Expression_Access;
     Mod_Semicolon_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Component_Clauses     : not null Program.Elements.Component_Clauses
         .Component_Clause_Vector_Access;
     Semicolon_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Record_Representation_Clause is
   begin
      return Result : Record_Representation_Clause :=
        (For_Token => For_Token, Name => Name, Use_Token => Use_Token,
         Record_Token => Record_Token, At_Token => At_Token,
         Mod_Token => Mod_Token,
         Mod_Clause_Expression => Mod_Clause_Expression,
         Mod_Semicolon_Token => Mod_Semicolon_Token,
         Component_Clauses => Component_Clauses,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Name                  : not null Program.Elements.Expressions
         .Expression_Access;
     Mod_Clause_Expression : Program.Elements.Expressions.Expression_Access;
     Component_Clauses     : not null Program.Elements.Component_Clauses
         .Component_Clause_Vector_Access;
     Is_Part_Of_Implicit   : Boolean := False;
     Is_Part_Of_Inherited  : Boolean := False;
     Is_Part_Of_Instance   : Boolean := False)
      return Implicit_Record_Representation_Clause is
   begin
      return Result : Implicit_Record_Representation_Clause :=
        (Name => Name, Mod_Clause_Expression => Mod_Clause_Expression,
         Component_Clauses => Component_Clauses,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Name
    (Self : Base_Record_Representation_Clause)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Name;
   end Name;

   overriding function Mod_Clause_Expression
    (Self : Base_Record_Representation_Clause)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Mod_Clause_Expression;
   end Mod_Clause_Expression;

   overriding function Component_Clauses
    (Self : Base_Record_Representation_Clause)
      return not null Program.Elements.Component_Clauses
          .Component_Clause_Vector_Access is
   begin
      return Self.Component_Clauses;
   end Component_Clauses;

   overriding function For_Token
    (Self : Record_Representation_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.For_Token;
   end For_Token;

   overriding function Use_Token
    (Self : Record_Representation_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Use_Token;
   end Use_Token;

   overriding function Record_Token
    (Self : Record_Representation_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Record_Token;
   end Record_Token;

   overriding function At_Token
    (Self : Record_Representation_Clause)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.At_Token;
   end At_Token;

   overriding function Mod_Token
    (Self : Record_Representation_Clause)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Mod_Token;
   end Mod_Token;

   overriding function Mod_Semicolon_Token
    (Self : Record_Representation_Clause)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Mod_Semicolon_Token;
   end Mod_Semicolon_Token;

   overriding function Semicolon_Token
    (Self : Record_Representation_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Record_Representation_Clause)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Record_Representation_Clause)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Record_Representation_Clause)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Record_Representation_Clause'Class) is
   begin
      Set_Enclosing_Element (Self.Name, Self'Unchecked_Access);
      if Self.Mod_Clause_Expression.Assigned then
         Set_Enclosing_Element
           (Self.Mod_Clause_Expression, Self'Unchecked_Access);
      end if;
      for Item in Self.Component_Clauses.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Record_Representation_Clause
    (Self : Base_Record_Representation_Clause)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Record_Representation_Clause;

   overriding function Is_Representation_Clause
    (Self : Base_Record_Representation_Clause)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Representation_Clause;

   overriding function Is_Clause
    (Self : Base_Record_Representation_Clause)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Clause;

   overriding procedure Visit
    (Self    : not null access Base_Record_Representation_Clause;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Record_Representation_Clause (Self);
   end Visit;

   overriding function To_Record_Representation_Clause_Text
    (Self : aliased in out Record_Representation_Clause)
      return Program.Elements.Record_Representation_Clauses
          .Record_Representation_Clause_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Record_Representation_Clause_Text;

   overriding function To_Record_Representation_Clause_Text
    (Self : aliased in out Implicit_Record_Representation_Clause)
      return Program.Elements.Record_Representation_Clauses
          .Record_Representation_Clause_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Record_Representation_Clause_Text;

end Program.Nodes.Record_Representation_Clauses;
