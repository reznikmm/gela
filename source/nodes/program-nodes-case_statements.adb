--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Case_Statements is

   function Create
    (Case_Token           : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Selecting_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Token             : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Paths                : not null Program.Elements.Case_Paths
         .Case_Path_Vector_Access;
     End_Token            : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Case_Token_2         : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Case_Statement is
   begin
      return Result : Case_Statement :=
        (Case_Token => Case_Token,
         Selecting_Expression => Selecting_Expression, Is_Token => Is_Token,
         Paths => Paths, End_Token => End_Token, Case_Token_2 => Case_Token_2,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Selecting_Expression : not null Program.Elements.Expressions
         .Expression_Access;
     Paths                : not null Program.Elements.Case_Paths
         .Case_Path_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Case_Statement is
   begin
      return Result : Implicit_Case_Statement :=
        (Selecting_Expression => Selecting_Expression, Paths => Paths,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Selecting_Expression
    (Self : Base_Case_Statement)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Selecting_Expression;
   end Selecting_Expression;

   overriding function Paths
    (Self : Base_Case_Statement)
      return not null Program.Elements.Case_Paths.Case_Path_Vector_Access is
   begin
      return Self.Paths;
   end Paths;

   overriding function Case_Token
    (Self : Case_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Case_Token;
   end Case_Token;

   overriding function Is_Token
    (Self : Case_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Is_Token;
   end Is_Token;

   overriding function End_Token
    (Self : Case_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.End_Token;
   end End_Token;

   overriding function Case_Token_2
    (Self : Case_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Case_Token_2;
   end Case_Token_2;

   overriding function Semicolon_Token
    (Self : Case_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Case_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Case_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Case_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Case_Statement'Class) is
   begin
      Set_Enclosing_Element (Self.Selecting_Expression, Self'Unchecked_Access);
      for Item in Self.Paths.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Case_Statement
    (Self : Base_Case_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Case_Statement;

   overriding function Is_Statement
    (Self : Base_Case_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Statement;

   overriding procedure Visit
    (Self    : not null access Base_Case_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Case_Statement (Self);
   end Visit;

   overriding function To_Case_Statement_Text
    (Self : aliased in out Case_Statement)
      return Program.Elements.Case_Statements.Case_Statement_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Case_Statement_Text;

   overriding function To_Case_Statement_Text
    (Self : aliased in out Implicit_Case_Statement)
      return Program.Elements.Case_Statements.Case_Statement_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Case_Statement_Text;

end Program.Nodes.Case_Statements;
