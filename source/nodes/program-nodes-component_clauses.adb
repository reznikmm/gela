--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Component_Clauses is

   function Create
    (Clause_Name     : not null Program.Elements.Identifiers.Identifier_Access;
     At_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Position        : not null Program.Elements.Expressions.Expression_Access;
     Range_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Clause_Range    : not null Program.Elements.Simple_Expression_Ranges
         .Simple_Expression_Range_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Component_Clause is
   begin
      return Result : Component_Clause :=
        (Clause_Name => Clause_Name, At_Token => At_Token,
         Position => Position, Range_Token => Range_Token,
         Clause_Range => Clause_Range, Semicolon_Token => Semicolon_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Clause_Name          : not null Program.Elements.Identifiers
         .Identifier_Access;
     Position             : not null Program.Elements.Expressions
         .Expression_Access;
     Clause_Range         : not null Program.Elements.Simple_Expression_Ranges
         .Simple_Expression_Range_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Component_Clause is
   begin
      return Result : Implicit_Component_Clause :=
        (Clause_Name => Clause_Name, Position => Position,
         Clause_Range => Clause_Range,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Clause_Name
    (Self : Base_Component_Clause)
      return not null Program.Elements.Identifiers.Identifier_Access is
   begin
      return Self.Clause_Name;
   end Clause_Name;

   overriding function Position
    (Self : Base_Component_Clause)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Position;
   end Position;

   overriding function Clause_Range
    (Self : Base_Component_Clause)
      return not null Program.Elements.Simple_Expression_Ranges
          .Simple_Expression_Range_Access is
   begin
      return Self.Clause_Range;
   end Clause_Range;

   overriding function At_Token
    (Self : Component_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.At_Token;
   end At_Token;

   overriding function Range_Token
    (Self : Component_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Range_Token;
   end Range_Token;

   overriding function Semicolon_Token
    (Self : Component_Clause)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Component_Clause)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Component_Clause)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Component_Clause)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Component_Clause'Class) is
   begin
      Set_Enclosing_Element (Self.Clause_Name, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Position, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Clause_Range, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Component_Clause
    (Self : Base_Component_Clause)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Component_Clause;

   overriding function Is_Clause
    (Self : Base_Component_Clause)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Clause;

   overriding procedure Visit
    (Self    : not null access Base_Component_Clause;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Component_Clause (Self);
   end Visit;

   overriding function To_Component_Clause_Text
    (Self : aliased in out Component_Clause)
      return Program.Elements.Component_Clauses.Component_Clause_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Component_Clause_Text;

   overriding function To_Component_Clause_Text
    (Self : aliased in out Implicit_Component_Clause)
      return Program.Elements.Component_Clauses.Component_Clause_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Component_Clause_Text;

end Program.Nodes.Component_Clauses;
