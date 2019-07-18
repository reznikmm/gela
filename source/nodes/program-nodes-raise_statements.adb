--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Raise_Statements is

   function Create
    (Raise_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Raised_Exception   : Program.Elements.Expressions.Expression_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Associated_Message : Program.Elements.Expressions.Expression_Access;
     Semicolon_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Raise_Statement is
   begin
      return Result : Raise_Statement :=
        (Raise_Token => Raise_Token, Raised_Exception => Raised_Exception,
         With_Token => With_Token, Associated_Message => Associated_Message,
         Semicolon_Token => Semicolon_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Raised_Exception     : Program.Elements.Expressions.Expression_Access;
     Associated_Message   : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Raise_Statement is
   begin
      return Result : Implicit_Raise_Statement :=
        (Raised_Exception => Raised_Exception,
         Associated_Message => Associated_Message,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Raised_Exception
    (Self : Base_Raise_Statement)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Raised_Exception;
   end Raised_Exception;

   overriding function Associated_Message
    (Self : Base_Raise_Statement)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Associated_Message;
   end Associated_Message;

   overriding function Raise_Token
    (Self : Raise_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Raise_Token;
   end Raise_Token;

   overriding function With_Token
    (Self : Raise_Statement)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Semicolon_Token
    (Self : Raise_Statement)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Semicolon_Token;
   end Semicolon_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Raise_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Raise_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Raise_Statement)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Raise_Statement'Class) is
   begin
      if Self.Raised_Exception.Assigned then
         Set_Enclosing_Element (Self.Raised_Exception, Self'Unchecked_Access);
      end if;
      if Self.Associated_Message.Assigned then
         Set_Enclosing_Element
           (Self.Associated_Message, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Raise_Statement
    (Self : Base_Raise_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Raise_Statement;

   overriding function Is_Statement
    (Self : Base_Raise_Statement)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Statement;

   overriding procedure Visit
    (Self    : not null access Base_Raise_Statement;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Raise_Statement (Self);
   end Visit;

   overriding function To_Raise_Statement_Text
    (Self : aliased in out Raise_Statement)
      return Program.Elements.Raise_Statements.Raise_Statement_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Raise_Statement_Text;

   overriding function To_Raise_Statement_Text
    (Self : aliased in out Implicit_Raise_Statement)
      return Program.Elements.Raise_Statements.Raise_Statement_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Raise_Statement_Text;

end Program.Nodes.Raise_Statements;
