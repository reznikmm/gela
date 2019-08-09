--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Abort_Statements is

   function Create
    (Abort_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Aborted_Tasks   : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Abort_Statement is
   begin
      return Result : Abort_Statement :=
        (Abort_Token => Abort_Token, Aborted_Tasks => Aborted_Tasks,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Aborted_Tasks        : not null Program.Elements.Expressions
         .Expression_Vector_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Abort_Statement is
   begin
      return Result : Implicit_Abort_Statement :=
        (Aborted_Tasks => Aborted_Tasks,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Aborted_Tasks
    (Self : Base_Abort_Statement)
      return not null Program.Elements.Expressions.Expression_Vector_Access is
   begin
      return Self.Aborted_Tasks;
   end Aborted_Tasks;

   overriding function Abort_Token
    (Self : Abort_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Abort_Token;
   end Abort_Token;

   overriding function Semicolon_Token
    (Self : Abort_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Abort_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Abort_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Abort_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Abort_Statement'Class) is
   begin
      for Item in Self.Aborted_Tasks.Each_Element loop
         Set_Enclosing_Element (Item.Element, Self'Unchecked_Access);
      end loop;
      null;
   end Initialize;

   overriding function Is_Abort_Statement_Element
    (Self : Base_Abort_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Abort_Statement_Element;

   overriding function Is_Statement_Element
    (Self : Base_Abort_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Statement_Element;

   overriding procedure Visit
    (Self    : not null access Base_Abort_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Abort_Statement (Self);
   end Visit;

   overriding function To_Abort_Statement_Text
    (Self : aliased in out Abort_Statement)
      return Program.Elements.Abort_Statements.Abort_Statement_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Abort_Statement_Text;

   overriding function To_Abort_Statement_Text
    (Self : aliased in out Implicit_Abort_Statement)
      return Program.Elements.Abort_Statements.Abort_Statement_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Abort_Statement_Text;

end Program.Nodes.Abort_Statements;
