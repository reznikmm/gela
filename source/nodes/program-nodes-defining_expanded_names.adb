--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Defining_Expanded_Names is

   function Create
    (Prefix    : not null Program.Elements.Expressions.Expression_Access;
     Dot_Token : not null Program.Lexical_Elements.Lexical_Element_Access;
     Selector  : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access)
      return Defining_Expanded_Name is
   begin
      return Result : Defining_Expanded_Name :=
        (Prefix => Prefix, Dot_Token => Dot_Token, Selector => Selector,
         Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Selector             : not null Program.Elements.Defining_Identifiers
         .Defining_Identifier_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Defining_Expanded_Name is
   begin
      return Result : Implicit_Defining_Expanded_Name :=
        (Prefix => Prefix, Selector => Selector,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Prefix
    (Self : Base_Defining_Expanded_Name)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Prefix;
   end Prefix;

   overriding function Selector
    (Self : Base_Defining_Expanded_Name)
      return not null Program.Elements.Defining_Identifiers
          .Defining_Identifier_Access is
   begin
      return Self.Selector;
   end Selector;

   overriding function Dot_Token
    (Self : Defining_Expanded_Name)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Dot_Token;
   end Dot_Token;

   overriding function Image (Self : Defining_Expanded_Name) return Text is
   begin
      return Self.Dot_Token.Image;
   end Image;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Defining_Expanded_Name)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Defining_Expanded_Name)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Defining_Expanded_Name)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   overriding function Image
    (Self : Implicit_Defining_Expanded_Name)
      return Text is
      pragma Unreferenced (Self);
   begin
      return "";
   end Image;

   procedure Initialize
    (Self : aliased in out Base_Defining_Expanded_Name'Class) is
   begin
      Set_Enclosing_Element (Self.Prefix, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Selector, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Defining_Expanded_Name
    (Self : Base_Defining_Expanded_Name)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Defining_Expanded_Name;

   overriding function Is_Defining_Name
    (Self : Base_Defining_Expanded_Name)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Defining_Name;

   overriding procedure Visit
    (Self    : not null access Base_Defining_Expanded_Name;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Defining_Expanded_Name (Self);
   end Visit;

   overriding function To_Defining_Expanded_Name_Text
    (Self : aliased in out Defining_Expanded_Name)
      return Program.Elements.Defining_Expanded_Names
          .Defining_Expanded_Name_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Defining_Expanded_Name_Text;

   overriding function To_Defining_Expanded_Name_Text
    (Self : aliased in out Implicit_Defining_Expanded_Name)
      return Program.Elements.Defining_Expanded_Names
          .Defining_Expanded_Name_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Defining_Expanded_Name_Text;

end Program.Nodes.Defining_Expanded_Names;
