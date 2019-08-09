--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Goto_Statements is

   function Create
    (Goto_Token      : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Goto_Label      : not null Program.Elements.Expressions.Expression_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Goto_Statement is
   begin
      return Result : Goto_Statement :=
        (Goto_Token => Goto_Token, Goto_Label => Goto_Label,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Goto_Label           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Goto_Statement is
   begin
      return Result : Implicit_Goto_Statement :=
        (Goto_Label => Goto_Label, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Goto_Label
    (Self : Base_Goto_Statement)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Goto_Label;
   end Goto_Label;

   overriding function Goto_Token
    (Self : Goto_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Goto_Token;
   end Goto_Token;

   overriding function Semicolon_Token
    (Self : Goto_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Goto_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Goto_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Goto_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Goto_Statement'Class) is
   begin
      Set_Enclosing_Element (Self.Goto_Label, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Goto_Statement_Element
    (Self : Base_Goto_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Goto_Statement_Element;

   overriding function Is_Statement_Element
    (Self : Base_Goto_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Statement_Element;

   overriding procedure Visit
    (Self    : not null access Base_Goto_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Goto_Statement (Self);
   end Visit;

   overriding function To_Goto_Statement_Text
    (Self : aliased in out Goto_Statement)
      return Program.Elements.Goto_Statements.Goto_Statement_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Goto_Statement_Text;

   overriding function To_Goto_Statement_Text
    (Self : aliased in out Implicit_Goto_Statement)
      return Program.Elements.Goto_Statements.Goto_Statement_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Goto_Statement_Text;

end Program.Nodes.Goto_Statements;
