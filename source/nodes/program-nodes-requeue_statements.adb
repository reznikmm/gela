--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Requeue_Statements is

   function Create
    (Requeue_Token   : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Entry_Name      : not null Program.Elements.Expressions.Expression_Access;
     With_Token      : Program.Lexical_Elements.Lexical_Element_Access;
     Abort_Token     : Program.Lexical_Elements.Lexical_Element_Access;
     Semicolon_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Requeue_Statement is
   begin
      return Result : Requeue_Statement :=
        (Requeue_Token => Requeue_Token, Entry_Name => Entry_Name,
         With_Token => With_Token, Abort_Token => Abort_Token,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Entry_Name           : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False;
     Has_With_Abort       : Boolean := False)
      return Implicit_Requeue_Statement is
   begin
      return Result : Implicit_Requeue_Statement :=
        (Entry_Name => Entry_Name, Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance,
         Has_With_Abort => Has_With_Abort, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Entry_Name
    (Self : Base_Requeue_Statement)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Entry_Name;
   end Entry_Name;

   overriding function Requeue_Token
    (Self : Requeue_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Requeue_Token;
   end Requeue_Token;

   overriding function With_Token
    (Self : Requeue_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Abort_Token
    (Self : Requeue_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Abort_Token;
   end Abort_Token;

   overriding function Semicolon_Token
    (Self : Requeue_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Has_With_Abort
    (Self : Requeue_Statement)
      return Boolean is
   begin
      return Self.With_Token.Assigned;
   end Has_With_Abort;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Requeue_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Requeue_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Requeue_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Has_With_Abort
    (Self : Implicit_Requeue_Statement)
      return Boolean is
   begin
      return Self.Has_With_Abort;
   end Has_With_Abort;

   procedure Initialize (Self : aliased in out Base_Requeue_Statement'Class) is
   begin
      Set_Enclosing_Element (Self.Entry_Name, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Requeue_Statement
    (Self : Base_Requeue_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Requeue_Statement;

   overriding function Is_Statement
    (Self : Base_Requeue_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Statement;

   overriding procedure Visit
    (Self    : not null access Base_Requeue_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Requeue_Statement (Self);
   end Visit;

   overriding function To_Requeue_Statement_Text
    (Self : aliased in out Requeue_Statement)
      return Program.Elements.Requeue_Statements
          .Requeue_Statement_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Requeue_Statement_Text;

   overriding function To_Requeue_Statement_Text
    (Self : aliased in out Implicit_Requeue_Statement)
      return Program.Elements.Requeue_Statements
          .Requeue_Statement_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Requeue_Statement_Text;

end Program.Nodes.Requeue_Statements;
