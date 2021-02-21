--  SPDX-FileCopyrightText: 2019 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
-------------------------------------------------------------

package body Program.Nodes.Identifiers is

   function Create
    (Identifier_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Identifier is
   begin
      return Result : Identifier :=
        (Identifier_Token => Identifier_Token,
         Corresponding_Defining_Identifier => null, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Identifier is
   begin
      return Result : Implicit_Identifier :=
        (Corresponding_Defining_Identifier => null,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Corresponding_Defining_Identifier
    (Self : Base_Identifier)
      return Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Self.Corresponding_Defining_Identifier;
   end Corresponding_Defining_Identifier;

   overriding function Identifier_Token
    (Self : Identifier)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Identifier_Token;
   end Identifier_Token;

   overriding function Image (Self : Identifier) return Text is
   begin
      return Self.Identifier_Token.Image;
   end Image;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Identifier)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Identifier)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Identifier)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Image (Self : Implicit_Identifier) return Text is
      pragma Unreferenced (Self);
   begin
      return "";
   end Image;

   procedure Initialize (Self : in out Base_Identifier'Class) is
   begin
      null;
   end Initialize;

   overriding function Is_Identifier (Self : Base_Identifier) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Identifier;

   overriding function Is_Expression (Self : Base_Identifier) return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression;

   overriding procedure Visit
    (Self    : not null access Base_Identifier;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Identifier (Self);
   end Visit;

   overriding function To_Identifier_Text
    (Self : in out Identifier)
      return Program.Elements.Identifiers.Identifier_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Identifier_Text;

   overriding function To_Identifier_Text
    (Self : in out Implicit_Identifier)
      return Program.Elements.Identifiers.Identifier_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Identifier_Text;

end Program.Nodes.Identifiers;
