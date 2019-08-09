--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Assignment_Statements is

   function Create
    (Variable_Name    : not null Program.Elements.Expressions
         .Expression_Access;
     Assignment_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression       : not null Program.Elements.Expressions
         .Expression_Access;
     Semicolon_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Assignment_Statement is
   begin
      return Result : Assignment_Statement :=
        (Variable_Name => Variable_Name, Assignment_Token => Assignment_Token,
         Expression => Expression, Semicolon_Token => Semicolon_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Variable_Name        : not null Program.Elements.Expressions
         .Expression_Access;
     Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Assignment_Statement is
   begin
      return Result : Implicit_Assignment_Statement :=
        (Variable_Name => Variable_Name, Expression => Expression,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Variable_Name
    (Self : Base_Assignment_Statement)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Variable_Name;
   end Variable_Name;

   overriding function Expression
    (Self : Base_Assignment_Statement)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Expression;
   end Expression;

   overriding function Assignment_Token
    (Self : Assignment_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Assignment_Token;
   end Assignment_Token;

   overriding function Semicolon_Token
    (Self : Assignment_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Assignment_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Assignment_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Assignment_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Assignment_Statement'Class) is
   begin
      Set_Enclosing_Element (Self.Variable_Name, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Expression, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Assignment_Statement_Element
    (Self : Base_Assignment_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Assignment_Statement_Element;

   overriding function Is_Statement_Element
    (Self : Base_Assignment_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Statement_Element;

   overriding procedure Visit
    (Self    : not null access Base_Assignment_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Assignment_Statement (Self);
   end Visit;

   overriding function To_Assignment_Statement_Text
    (Self : aliased in out Assignment_Statement)
      return Program.Elements.Assignment_Statements
          .Assignment_Statement_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Assignment_Statement_Text;

   overriding function To_Assignment_Statement_Text
    (Self : aliased in out Implicit_Assignment_Statement)
      return Program.Elements.Assignment_Statements
          .Assignment_Statement_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Assignment_Statement_Text;

end Program.Nodes.Assignment_Statements;
