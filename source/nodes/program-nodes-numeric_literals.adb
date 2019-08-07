--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Numeric_Literals is

   function Create
    (Numeric_Literal_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Numeric_Literal is
   begin
      return Result : Numeric_Literal :=
        (Numeric_Literal_Token => Numeric_Literal_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Numeric_Literal is
   begin
      return Result : Implicit_Numeric_Literal :=
        (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Numeric_Literal_Token
    (Self : Numeric_Literal)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Numeric_Literal_Token;
   end Numeric_Literal_Token;

   overriding function Image (Self : Numeric_Literal) return Text is
   begin
      return Self.Numeric_Literal_Token.Image;
   end Image;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Numeric_Literal)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Numeric_Literal)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Numeric_Literal)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Image (Self : Implicit_Numeric_Literal) return Text is
      pragma Unreferenced (Self);
   begin
      return "";
   end Image;

   procedure Initialize (Self : aliased in out Base_Numeric_Literal'Class) is
   begin
      null;
   end Initialize;

   overriding function Is_Numeric_Literal
    (Self : Base_Numeric_Literal)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Numeric_Literal;

   overriding function Is_Expression
    (Self : Base_Numeric_Literal)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression;

   overriding procedure Visit
    (Self    : not null access Base_Numeric_Literal;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Numeric_Literal (Self);
   end Visit;

   overriding function To_Numeric_Literal_Text
    (Self : aliased in out Numeric_Literal)
      return Program.Elements.Numeric_Literals.Numeric_Literal_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Numeric_Literal_Text;

   overriding function To_Numeric_Literal_Text
    (Self : aliased in out Implicit_Numeric_Literal)
      return Program.Elements.Numeric_Literals.Numeric_Literal_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Numeric_Literal_Text;

end Program.Nodes.Numeric_Literals;
