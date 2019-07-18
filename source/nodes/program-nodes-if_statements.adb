--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.If_Statements is

   function Create
    (If_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Condition       : not null Program.Elements.Expressions.Expression_Access;
     Then_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Then_Statements : not null Program.Element_Vectors.Element_Vector_Access;
     Elsif_Paths     : not null Program.Elements.Elsif_Paths
         .Elsif_Path_Vector_Access;
     Else_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Else_Statements : not null Program.Element_Vectors.Element_Vector_Access;
     End_Token       : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     If_Token_2      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return If_Statement is
   begin
      return Result : If_Statement :=
        (If_Token => If_Token, Condition => Condition,
         Then_Token => Then_Token, Then_Statements => Then_Statements,
         Elsif_Paths => Elsif_Paths, Else_Token => Else_Token,
         Else_Statements => Else_Statements, End_Token => End_Token,
         If_Token_2 => If_Token_2, Semicolon_Token => Semicolon_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Condition            : not null Program.Elements.Expressions
         .Expression_Access;
     Then_Statements      : not null Program.Element_Vectors
         .Element_Vector_Access;
     Elsif_Paths          : not null Program.Elements.Elsif_Paths
         .Elsif_Path_Vector_Access;
     Else_Statements      : not null Program.Element_Vectors
         .Element_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_If_Statement is
   begin
      return Result : Implicit_If_Statement :=
        (Condition => Condition, Then_Statements => Then_Statements,
         Elsif_Paths => Elsif_Paths, Else_Statements => Else_Statements,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Condition
    (Self : Base_If_Statement)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Condition;
   end Condition;

   overriding function Then_Statements
    (Self : Base_If_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Then_Statements;
   end Then_Statements;

   overriding function Elsif_Paths
    (Self : Base_If_Statement)
      return not null Program.Elements.Elsif_Paths.Elsif_Path_Vector_Access is
   begin
      return Self.Elsif_Paths;
   end Elsif_Paths;

   overriding function Else_Statements
    (Self : Base_If_Statement)
      return not null Program.Element_Vectors.Element_Vector_Access is
   begin
      return Self.Else_Statements;
   end Else_Statements;

   overriding function If_Token
    (Self : If_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.If_Token;
   end If_Token;

   overriding function Then_Token
    (Self : If_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Then_Token;
   end Then_Token;

   overriding function Else_Token
    (Self : If_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Else_Token;
   end Else_Token;

   overriding function End_Token
    (Self : If_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.End_Token;
   end End_Token;

   overriding function If_Token_2
    (Self : If_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.If_Token_2;
   end If_Token_2;

   overriding function Semicolon_Token
    (Self : If_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_If_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_If_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_If_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_If_Statement'Class) is
   begin
      Set_Enclosing_Element (Self.Condition, Self'Unchecked_Access);
      for Item in Self.Then_Statements.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Elsif_Paths.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      for Item in Self.Else_Statements.Each loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_If_Statement
    (Self : Base_If_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_If_Statement;

   overriding function Is_Statement
    (Self : Base_If_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Statement;

   overriding procedure Visit
    (Self    : not null access Base_If_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.If_Statement (Self);
   end Visit;

   overriding function To_If_Statement_Text
    (Self : aliased in out If_Statement)
      return Program.Elements.If_Statements.If_Statement_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_If_Statement_Text;

   overriding function To_If_Statement_Text
    (Self : aliased in out Implicit_If_Statement)
      return Program.Elements.If_Statements.If_Statement_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_If_Statement_Text;

end Program.Nodes.If_Statements;
