--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Operator_Symbols is

   function Create
    (Operator_Symbol_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Operator_Symbol is
   begin
      return Result : Operator_Symbol :=
        (Operator_Symbol_Token => Operator_Symbol_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Operator_Symbol is
   begin
      return Result : Implicit_Operator_Symbol :=
        (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Operator_Symbol_Token
    (Self : Operator_Symbol)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Operator_Symbol_Token;
   end Operator_Symbol_Token;

   overriding function Image (Self : Operator_Symbol) return Text is
   begin
      return Self.Operator_Symbol_Token.Image;
   end Image;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Operator_Symbol)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Operator_Symbol)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Operator_Symbol)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Image (Self : Implicit_Operator_Symbol) return Text is
      pragma Unreferenced (Self);
   begin
      return "";
   end Image;

   procedure Initialize (Self : aliased in out Base_Operator_Symbol'Class) is
   begin
      null;
   end Initialize;

   overriding function Is_Operator_Symbol_Element
    (Self : Base_Operator_Symbol)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Operator_Symbol_Element;

   overriding function Is_Expression_Element
    (Self : Base_Operator_Symbol)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression_Element;

   overriding procedure Visit
    (Self    : not null access Base_Operator_Symbol;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Operator_Symbol (Self);
   end Visit;

   overriding function To_Operator_Symbol_Text
    (Self : aliased in out Operator_Symbol)
      return Program.Elements.Operator_Symbols.Operator_Symbol_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Operator_Symbol_Text;

   overriding function To_Operator_Symbol_Text
    (Self : aliased in out Implicit_Operator_Symbol)
      return Program.Elements.Operator_Symbols.Operator_Symbol_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Operator_Symbol_Text;

end Program.Nodes.Operator_Symbols;
