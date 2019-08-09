--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Null_Literals is

   function Create
    (Null_Literal_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Null_Literal is
   begin
      return Result : Null_Literal :=
        (Null_Literal_Token => Null_Literal_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Null_Literal is
   begin
      return Result : Implicit_Null_Literal :=
        (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Null_Literal_Token
    (Self : Null_Literal)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Null_Literal_Token;
   end Null_Literal_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Null_Literal)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Null_Literal)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Null_Literal)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_Null_Literal'Class) is
   begin
      null;
   end Initialize;

   overriding function Is_Null_Literal_Element
    (Self : Base_Null_Literal)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Null_Literal_Element;

   overriding function Is_Expression_Element
    (Self : Base_Null_Literal)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression_Element;

   overriding procedure Visit
    (Self    : not null access Base_Null_Literal;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Null_Literal (Self);
   end Visit;

   overriding function To_Null_Literal_Text
    (Self : aliased in out Null_Literal)
      return Program.Elements.Null_Literals.Null_Literal_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Null_Literal_Text;

   overriding function To_Null_Literal_Text
    (Self : aliased in out Implicit_Null_Literal)
      return Program.Elements.Null_Literals.Null_Literal_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Null_Literal_Text;

end Program.Nodes.Null_Literals;
