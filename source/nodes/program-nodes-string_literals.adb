--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.String_Literals is

   function Create
    (String_Literal_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return String_Literal is
   begin
      return Result : String_Literal :=
        (String_Literal_Token => String_Literal_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_String_Literal is
   begin
      return Result : Implicit_String_Literal :=
        (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function String_Literal_Token
    (Self : String_Literal)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.String_Literal_Token;
   end String_Literal_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_String_Literal)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_String_Literal)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_String_Literal)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize (Self : aliased in out Base_String_Literal'Class) is
   begin
      null;
   end Initialize;

   overriding function Is_String_Literal
    (Self : Base_String_Literal)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_String_Literal;

   overriding function Is_Expression
    (Self : Base_String_Literal)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression;

   overriding procedure Visit
    (Self    : not null access Base_String_Literal;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.String_Literal (Self);
   end Visit;

   overriding function To_String_Literal_Text
    (Self : aliased in out String_Literal)
      return Program.Elements.String_Literals.String_Literal_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_String_Literal_Text;

   overriding function To_String_Literal_Text
    (Self : aliased in out Implicit_String_Literal)
      return Program.Elements.String_Literals.String_Literal_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_String_Literal_Text;

end Program.Nodes.String_Literals;
