--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Simple_Return_Statements is

   function Create
    (Return_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression      : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Simple_Return_Statement is
   begin
      return Result : Simple_Return_Statement :=
        (Return_Token => Return_Token, Expression => Expression,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Expression           : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Simple_Return_Statement is
   begin
      return Result : Implicit_Simple_Return_Statement :=
        (Expression => Expression, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Expression
    (Self : Base_Simple_Return_Statement)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Expression;
   end Expression;

   overriding function Return_Token
    (Self : Simple_Return_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Return_Token;
   end Return_Token;

   overriding function Semicolon_Token
    (Self : Simple_Return_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Simple_Return_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Simple_Return_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Simple_Return_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Simple_Return_Statement'Class) is
   begin
      if Self.Expression.Assigned then
         Set_Enclosing_Element (Self.Expression, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Simple_Return_Statement_Element
    (Self : Base_Simple_Return_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Simple_Return_Statement_Element;

   overriding function Is_Statement_Element
    (Self : Base_Simple_Return_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Statement_Element;

   overriding procedure Visit
    (Self    : not null access Base_Simple_Return_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Simple_Return_Statement (Self);
   end Visit;

   overriding function To_Simple_Return_Statement_Text
    (Self : aliased in out Simple_Return_Statement)
      return Program.Elements.Simple_Return_Statements
          .Simple_Return_Statement_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Simple_Return_Statement_Text;

   overriding function To_Simple_Return_Statement_Text
    (Self : aliased in out Implicit_Simple_Return_Statement)
      return Program.Elements.Simple_Return_Statements
          .Simple_Return_Statement_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Simple_Return_Statement_Text;

end Program.Nodes.Simple_Return_Statements;
