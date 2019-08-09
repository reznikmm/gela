--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Code_Statements is

   function Create
    (Expression      : not null Program.Elements.Qualified_Expressions
         .Qualified_Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Code_Statement is
   begin
      return Result : Code_Statement :=
        (Expression => Expression, Semicolon_Token => Semicolon_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Expression           : not null Program.Elements.Qualified_Expressions
         .Qualified_Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Code_Statement is
   begin
      return Result : Implicit_Code_Statement :=
        (Expression => Expression, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Expression
    (Self : Base_Code_Statement)
      return not null Program.Elements.Qualified_Expressions
          .Qualified_Expression_Access is
   begin
      return Self.Expression;
   end Expression;

   overriding function Semicolon_Token
    (Self : Code_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Code_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Code_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Code_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Code_Statement'Class) is
   begin
      Set_Enclosing_Element (Self.Expression, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Code_Statement_Element
    (Self : Base_Code_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Code_Statement_Element;

   overriding function Is_Statement_Element
    (Self : Base_Code_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Statement_Element;

   overriding procedure Visit
    (Self    : not null access Base_Code_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Code_Statement (Self);
   end Visit;

   overriding function To_Code_Statement_Text
    (Self : aliased in out Code_Statement)
      return Program.Elements.Code_Statements.Code_Statement_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Code_Statement_Text;

   overriding function To_Code_Statement_Text
    (Self : aliased in out Implicit_Code_Statement)
      return Program.Elements.Code_Statements.Code_Statement_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Code_Statement_Text;

end Program.Nodes.Code_Statements;
