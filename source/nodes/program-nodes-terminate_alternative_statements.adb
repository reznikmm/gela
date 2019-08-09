--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Terminate_Alternative_Statements is

   function Create
    (Terminate_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Terminate_Alternative_Statement is
   begin
      return Result : Terminate_Alternative_Statement :=
        (Terminate_Token => Terminate_Token,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Terminate_Alternative_Statement is
   begin
      return Result : Implicit_Terminate_Alternative_Statement :=
        (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Terminate_Token
    (Self : Terminate_Alternative_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Terminate_Token;
   end Terminate_Token;

   overriding function Semicolon_Token
    (Self : Terminate_Alternative_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Terminate_Alternative_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Terminate_Alternative_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Terminate_Alternative_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Terminate_Alternative_Statement'Class) is
   begin
      null;
   end Initialize;

   overriding function Is_Terminate_Alternative_Statement_Element
    (Self : Base_Terminate_Alternative_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Terminate_Alternative_Statement_Element;

   overriding function Is_Statement_Element
    (Self : Base_Terminate_Alternative_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Statement_Element;

   overriding procedure Visit
    (Self    : not null access Base_Terminate_Alternative_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Terminate_Alternative_Statement (Self);
   end Visit;

   overriding function To_Terminate_Alternative_Statement_Text
    (Self : aliased in out Terminate_Alternative_Statement)
      return Program.Elements.Terminate_Alternative_Statements
          .Terminate_Alternative_Statement_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Terminate_Alternative_Statement_Text;

   overriding function To_Terminate_Alternative_Statement_Text
    (Self : aliased in out Implicit_Terminate_Alternative_Statement)
      return Program.Elements.Terminate_Alternative_Statements
          .Terminate_Alternative_Statement_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Terminate_Alternative_Statement_Text;

end Program.Nodes.Terminate_Alternative_Statements;
