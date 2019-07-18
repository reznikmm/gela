--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Defining_Character_Literals is

   function Create
    (Character_Literal_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Defining_Character_Literal is
   begin
      return Result : Defining_Character_Literal :=
        (Character_Literal_Token => Character_Literal_Token,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Defining_Character_Literal is
   begin
      return Result : Implicit_Defining_Character_Literal :=
        (Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Character_Literal_Token
    (Self : Defining_Character_Literal)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Character_Literal_Token;
   end Character_Literal_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Defining_Character_Literal)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Defining_Character_Literal)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Defining_Character_Literal)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Defining_Character_Literal'Class) is
   begin
      null;
   end Initialize;

   overriding function Is_Defining_Character_Literal
    (Self : Base_Defining_Character_Literal)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Defining_Character_Literal;

   overriding function Is_Defining_Name
    (Self : Base_Defining_Character_Literal)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Defining_Name;

   overriding procedure Visit
    (Self    : not null access Base_Defining_Character_Literal;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Defining_Character_Literal (Self);
   end Visit;

   overriding function To_Defining_Character_Literal_Text
    (Self : aliased in out Defining_Character_Literal)
      return Program.Elements.Defining_Character_Literals
          .Defining_Character_Literal_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Defining_Character_Literal_Text;

   overriding function To_Defining_Character_Literal_Text
    (Self : aliased in out Implicit_Defining_Character_Literal)
      return Program.Elements.Defining_Character_Literals
          .Defining_Character_Literal_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Defining_Character_Literal_Text;

end Program.Nodes.Defining_Character_Literals;
