--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Short_Circuit_Operations is

   function Create
    (Left       : not null Program.Elements.Expressions.Expression_Access;
     And_Token  : Program.Lexical_Elements.Lexical_Element_Access;
     Then_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Or_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Else_Token : Program.Lexical_Elements.Lexical_Element_Access;
     Right      : not null Program.Elements.Expressions.Expression_Access)
      return Short_Circuit_Operation is
   begin
      return Result : Short_Circuit_Operation :=
        (Left => Left, And_Token => And_Token, Then_Token => Then_Token,
         Or_Token => Or_Token, Else_Token => Else_Token, Right => Right,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Left                 : not null Program.Elements.Expressions
         .Expression_Access;
     Right                : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_And_Then         : Boolean := False;
     Has_Or_Else          : Boolean := False)
      return Implicit_Short_Circuit_Operation is
   begin
      return Result : Implicit_Short_Circuit_Operation :=
        (Left => Left, Right => Right,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_And_Then => Has_And_Then, Has_Or_Else => Has_Or_Else,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Left
    (Self : Base_Short_Circuit_Operation)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Left;
   end Left;

   overriding function Right
    (Self : Base_Short_Circuit_Operation)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Right;
   end Right;

   overriding function And_Token
    (Self : Short_Circuit_Operation)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.And_Token;
   end And_Token;

   overriding function Then_Token
    (Self : Short_Circuit_Operation)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Then_Token;
   end Then_Token;

   overriding function Or_Token
    (Self : Short_Circuit_Operation)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Or_Token;
   end Or_Token;

   overriding function Else_Token
    (Self : Short_Circuit_Operation)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Else_Token;
   end Else_Token;

   overriding function Has_And_Then
    (Self : Short_Circuit_Operation)
      return Boolean is
   begin
      return Self.And_Then_Token.Assigned;
   end Has_And_Then;

   overriding function Has_Or_Else
    (Self : Short_Circuit_Operation)
      return Boolean is
   begin
      return Self.Or_Else_Token.Assigned;
   end Has_Or_Else;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Short_Circuit_Operation)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Short_Circuit_Operation)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Short_Circuit_Operation)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_And_Then
    (Self : Implicit_Short_Circuit_Operation)
      return Boolean is
   begin
      return Self.Has_And_Then;
   end Has_And_Then;

   overriding function Has_Or_Else
    (Self : Implicit_Short_Circuit_Operation)
      return Boolean is
   begin
      return Self.Has_Or_Else;
   end Has_Or_Else;

   procedure Initialize
    (Self : aliased in out Base_Short_Circuit_Operation'Class) is
   begin
      Set_Enclosing_Element (Self.Left, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Right, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Short_Circuit_Operation
    (Self : Base_Short_Circuit_Operation)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Short_Circuit_Operation;

   overriding function Is_Expression
    (Self : Base_Short_Circuit_Operation)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression;

   overriding procedure Visit
    (Self    : not null access Base_Short_Circuit_Operation;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Short_Circuit_Operation (Self);
   end Visit;

   overriding function To_Short_Circuit_Operation_Text
    (Self : aliased in out Short_Circuit_Operation)
      return Program.Elements.Short_Circuit_Operations
          .Short_Circuit_Operation_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Short_Circuit_Operation_Text;

   overriding function To_Short_Circuit_Operation_Text
    (Self : aliased in out Implicit_Short_Circuit_Operation)
      return Program.Elements.Short_Circuit_Operations
          .Short_Circuit_Operation_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Short_Circuit_Operation_Text;

end Program.Nodes.Short_Circuit_Operations;
