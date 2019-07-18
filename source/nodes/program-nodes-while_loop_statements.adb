--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.While_Loop_Statements is

   function Create
    (Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Colon_Token              : Program.Lexical_Elements
         .Lexical_Element_Access;
     While_Token              : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Condition                : not null Program.Elements.Expressions
         .Expression_Access;
     Loop_Token               : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Token                : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Loop_Token_2             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Semicolon_Token          : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return While_Loop_Statement is
   begin
      return Result : While_Loop_Statement :=
        (Statement_Identifier => Statement_Identifier,
         Colon_Token => Colon_Token, While_Token => While_Token,
         Condition => Condition, Loop_Token => Loop_Token,
         Statements => Statements, End_Token => End_Token,
         Loop_Token_2 => Loop_Token_2,
         End_Statement_Identifier => End_Statement_Identifier,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Statement_Identifier     : Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Condition                : not null Program.Elements.Expressions
         .Expression_Access;
     Statements               : not null Program.Element_Vectors
         .Element_Vector_Access;
     End_Statement_Identifier : Program.Elements.Identifiers.Identifier_Access;
     Is_Part_Of_Implicit      : Boolean := False;
     Is_Part_Of_Inherited     : Boolean := False;
     Is_Part_Of_Instance      : Boolean := False)
      return Implicit_While_Loop_Statement is
   begin
      return Result : Implicit_While_Loop_Statement :=
        (Statement_Identifier => Statement_Identifier, Condition => Condition,
         Statements => Statements,
         End_Statement_Identifier => End_Statement_Identifier,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Statement_Identifier
    (Self : Base_While_Loop_Statement)
      return Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Self.Statement_Identifier;
   end Statement_Identifier;

   overriding function Condition
    (Self : Base_While_Loop_Statement)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Condition;
   end Condition;

   overriding function Statements
    (Self : Base_While_Loop_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Statements;
   end Statements;

   overriding function End_Statement_Identifier
    (Self : Base_While_Loop_Statement)
      return Program.Elements.Identifiers.Identifier_Access is
   begin
      return Self.End_Statement_Identifier;
   end End_Statement_Identifier;

   overriding function Colon_Token
    (Self : While_Loop_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Colon_Token;
   end Colon_Token;

   overriding function While_Token
    (Self : While_Loop_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.While_Token;
   end While_Token;

   overriding function Loop_Token
    (Self : While_Loop_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Loop_Token;
   end Loop_Token;

   overriding function End_Token
    (Self : While_Loop_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.End_Token;
   end End_Token;

   overriding function Loop_Token_2
    (Self : While_Loop_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Loop_Token_2;
   end Loop_Token_2;

   overriding function Semicolon_Token
    (Self : While_Loop_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_While_Loop_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_While_Loop_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_While_Loop_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_While_Loop_Statement'Class) is
   begin
      if Self.Statement_Identifier.Assigned then
         Set_Enclosing_Element
           (Self.Statement_Identifier, Self'Unchecked_Access);
      end if;
      Set_Enclosing_Element (Self.Condition, Self'Unchecked_Access);
      for Item in Self.Statements.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      if Self.End_Statement_Identifier.Assigned then
         Set_Enclosing_Element
           (Self.End_Statement_Identifier, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_While_Loop_Statement
    (Self : Base_While_Loop_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_While_Loop_Statement;

   overriding function Is_Statement
    (Self : Base_While_Loop_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Statement;

   overriding procedure Visit
    (Self    : not null access Base_While_Loop_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.While_Loop_Statement (Self);
   end Visit;

   overriding function To_While_Loop_Statement_Text
    (Self : aliased in out While_Loop_Statement)
      return Program.Elements.While_Loop_Statements
          .While_Loop_Statement_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_While_Loop_Statement_Text;

   overriding function To_While_Loop_Statement_Text
    (Self : aliased in out Implicit_While_Loop_Statement)
      return Program.Elements.While_Loop_Statements
          .While_Loop_Statement_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_While_Loop_Statement_Text;

end Program.Nodes.While_Loop_Statements;
