--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Raise_Expressions is

   function Create
    (Raise_Token        : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Exception_Name     : not null Program.Elements.Expressions
         .Expression_Access;
     With_Token         : Program.Lexical_Elements.Lexical_Element_Access;
     Associated_Message : Program.Elements.Expressions.Expression_Access)
      return Raise_Expression is
   begin
      return Result : Raise_Expression :=
        (Raise_Token => Raise_Token, Exception_Name => Exception_Name,
         With_Token => With_Token, Associated_Message => Associated_Message,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Exception_Name       : not null Program.Elements.Expressions
         .Expression_Access;
     Associated_Message   : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Raise_Expression is
   begin
      return Result : Implicit_Raise_Expression :=
        (Exception_Name => Exception_Name,
         Associated_Message => Associated_Message,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Exception_Name
    (Self : Base_Raise_Expression)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Exception_Name;
   end Exception_Name;

   overriding function Associated_Message
    (Self : Base_Raise_Expression)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Associated_Message;
   end Associated_Message;

   overriding function Raise_Token
    (Self : Raise_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Raise_Token;
   end Raise_Token;

   overriding function With_Token
    (Self : Raise_Expression)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.With_Token;
   end With_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Raise_Expression)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Raise_Expression)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Raise_Expression)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Raise_Expression'Class) is
   begin
      Set_Enclosing_Element (Self.Exception_Name, Self'Unchecked_Access);
      if Self.Associated_Message.Assigned then
         Set_Enclosing_Element
           (Self.Associated_Message, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Raise_Expression_Element
    (Self : Base_Raise_Expression)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Raise_Expression_Element;

   overriding function Is_Expression_Element
    (Self : Base_Raise_Expression)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression_Element;

   overriding procedure Visit
    (Self    : not null access Base_Raise_Expression;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Raise_Expression (Self);
   end Visit;

   overriding function To_Raise_Expression_Text
    (Self : aliased in out Raise_Expression)
      return Program.Elements.Raise_Expressions.Raise_Expression_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Raise_Expression_Text;

   overriding function To_Raise_Expression_Text
    (Self : aliased in out Implicit_Raise_Expression)
      return Program.Elements.Raise_Expressions.Raise_Expression_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Raise_Expression_Text;

end Program.Nodes.Raise_Expressions;
