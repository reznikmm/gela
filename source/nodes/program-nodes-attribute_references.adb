--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Attribute_References is

   function Create
    (Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Apostrophe_Token     : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Attribute_Designator : not null Program.Elements.Identifiers
         .Identifier_Access;
     Left_Bracket_Token   : Program.Lexical_Elements.Lexical_Element_Access;
     Expressions          : Program.Elements.Expressions.Expression_Access;
     Right_Bracket_Token  : Program.Lexical_Elements.Lexical_Element_Access)
      return Attribute_Reference is
   begin
      return Result : Attribute_Reference :=
        (Prefix => Prefix, Apostrophe_Token => Apostrophe_Token,
         Attribute_Designator => Attribute_Designator,
         Left_Bracket_Token => Left_Bracket_Token, Expressions => Expressions,
         Right_Bracket_Token => Right_Bracket_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Prefix               : not null Program.Elements.Expressions
         .Expression_Access;
     Attribute_Designator : not null Program.Elements.Identifiers
         .Identifier_Access;
     Expressions          : Program.Elements.Expressions.Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Attribute_Reference is
   begin
      return Result : Implicit_Attribute_Reference :=
        (Prefix => Prefix, Attribute_Designator => Attribute_Designator,
         Expressions => Expressions,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Prefix
    (Self : Base_Attribute_Reference)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Prefix;
   end Prefix;

   overriding function Attribute_Designator
    (Self : Base_Attribute_Reference)
      return not null Program.Elements.Identifiers.Identifier_Access is
   begin
      return Self.Attribute_Designator;
   end Attribute_Designator;

   overriding function Expressions
    (Self : Base_Attribute_Reference)
      return Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Expressions;
   end Expressions;

   overriding function Apostrophe_Token
    (Self : Attribute_Reference)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Apostrophe_Token;
   end Apostrophe_Token;

   overriding function Left_Bracket_Token
    (Self : Attribute_Reference)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Attribute_Reference)
      return Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Attribute_Reference)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Attribute_Reference)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Attribute_Reference)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Attribute_Reference'Class) is
   begin
      Set_Enclosing_Element (Self.Prefix, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Attribute_Designator, Self'Unchecked_Access);
      if Self.Expressions.Assigned then
         Set_Enclosing_Element (Self.Expressions, Self'Unchecked_Access);
      end if;
      null;
   end Initialize;

   overriding function Is_Attribute_Reference_Element
    (Self : Base_Attribute_Reference)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Attribute_Reference_Element;

   overriding function Is_Expression_Element
    (Self : Base_Attribute_Reference)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression_Element;

   overriding procedure Visit
    (Self    : not null access Base_Attribute_Reference;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Attribute_Reference (Self);
   end Visit;

   overriding function To_Attribute_Reference_Text
    (Self : aliased in out Attribute_Reference)
      return Program.Elements.Attribute_References
          .Attribute_Reference_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Attribute_Reference_Text;

   overriding function To_Attribute_Reference_Text
    (Self : aliased in out Implicit_Attribute_Reference)
      return Program.Elements.Attribute_References
          .Attribute_Reference_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Attribute_Reference_Text;

end Program.Nodes.Attribute_References;
