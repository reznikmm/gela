--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Nodes.Parenthesized_Expressions is

   function Create
    (Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Expression          : not null Program.Elements.Expressions
         .Expression_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Parenthesized_Expression is
   begin
      return Result : Parenthesized_Expression :=
        (Left_Bracket_Token => Left_Bracket_Token, Expression => Expression,
         Right_Bracket_Token => Right_Bracket_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Expression           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Parenthesized_Expression is
   begin
      return Result : Implicit_Parenthesized_Expression :=
        (Expression => Expression, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Expression
    (Self : Base_Parenthesized_Expression)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Expression;
   end Expression;

   overriding function Left_Bracket_Token
    (Self : Parenthesized_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Parenthesized_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Parenthesized_Expression)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Parenthesized_Expression)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Parenthesized_Expression)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Parenthesized_Expression'Class) is
   begin
      Set_Enclosing_Element (Self.Expression, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Parenthesized_Expression_Element
    (Self : Base_Parenthesized_Expression)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Parenthesized_Expression_Element;

   overriding function Is_Expression_Element
    (Self : Base_Parenthesized_Expression)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression_Element;

   overriding procedure Visit
    (Self    : not null access Base_Parenthesized_Expression;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Parenthesized_Expression (Self);
   end Visit;

   overriding function To_Parenthesized_Expression_Text
    (Self : aliased in out Parenthesized_Expression)
      return Program.Elements.Parenthesized_Expressions
          .Parenthesized_Expression_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Parenthesized_Expression_Text;

   overriding function To_Parenthesized_Expression_Text
    (Self : aliased in out Implicit_Parenthesized_Expression)
      return Program.Elements.Parenthesized_Expressions
          .Parenthesized_Expression_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Parenthesized_Expression_Text;

end Program.Nodes.Parenthesized_Expressions;
