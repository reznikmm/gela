--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Program.Nodes.Qualified_Expressions is

   function Create
    (Subtype_Mark        : not null Program.Elements.Expressions
         .Expression_Access;
     Apostrophe_Token    : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Left_Bracket_Token  : not null Program.Lexical_Elements
         .Lexical_Element_Access;
     Operand             : not null Program.Elements.Expressions
         .Expression_Access;
     Right_Bracket_Token : not null Program.Lexical_Elements
         .Lexical_Element_Access)
      return Qualified_Expression is
   begin
      return Result : Qualified_Expression :=
        (Subtype_Mark => Subtype_Mark, Apostrophe_Token => Apostrophe_Token,
         Left_Bracket_Token => Left_Bracket_Token, Operand => Operand,
         Right_Bracket_Token => Right_Bracket_Token, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   function Create
    (Subtype_Mark         : not null Program.Elements.Expressions
         .Expression_Access;
     Operand              : not null Program.Elements.Expressions
         .Expression_Access;
     Is_Part_Of_Implicit  : Boolean := False;
     Is_Part_Of_Inherited : Boolean := False;
     Is_Part_Of_Instance  : Boolean := False)
      return Implicit_Qualified_Expression is
   begin
      return Result : Implicit_Qualified_Expression :=
        (Subtype_Mark => Subtype_Mark, Operand => Operand,
         Is_Part_Of_Implicit => Is_Part_Of_Implicit,
         Is_Part_Of_Inherited => Is_Part_Of_Inherited,
         Is_Part_Of_Instance => Is_Part_Of_Instance, Enclosing_Element => null)
      do
         Initialize (Result);
      end return;
   end Create;

   overriding function Subtype_Mark
    (Self : Base_Qualified_Expression)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Subtype_Mark;
   end Subtype_Mark;

   overriding function Operand
    (Self : Base_Qualified_Expression)
      return not null Program.Elements.Expressions.Expression_Access is
   begin
      return Self.Operand;
   end Operand;

   overriding function Apostrophe_Token
    (Self : Qualified_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Apostrophe_Token;
   end Apostrophe_Token;

   overriding function Left_Bracket_Token
    (Self : Qualified_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Left_Bracket_Token;
   end Left_Bracket_Token;

   overriding function Right_Bracket_Token
    (Self : Qualified_Expression)
      return not null Program.Lexical_Elements.Lexical_Element_Access is
   begin
      return Self.Right_Bracket_Token;
   end Right_Bracket_Token;

   overriding function Is_Part_Of_Implicit
    (Self : Implicit_Qualified_Expression)
      return Boolean is
   begin
      return Self.Is_Part_Of_Implicit;
   end Is_Part_Of_Implicit;

   overriding function Is_Part_Of_Inherited
    (Self : Implicit_Qualified_Expression)
      return Boolean is
   begin
      return Self.Is_Part_Of_Inherited;
   end Is_Part_Of_Inherited;

   overriding function Is_Part_Of_Instance
    (Self : Implicit_Qualified_Expression)
      return Boolean is
   begin
      return Self.Is_Part_Of_Instance;
   end Is_Part_Of_Instance;

   procedure Initialize
    (Self : aliased in out Base_Qualified_Expression'Class) is
   begin
      Set_Enclosing_Element (Self.Subtype_Mark, Self'Unchecked_Access);
      Set_Enclosing_Element (Self.Operand, Self'Unchecked_Access);
      null;
   end Initialize;

   overriding function Is_Qualified_Expression_Element
    (Self : Base_Qualified_Expression)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Qualified_Expression_Element;

   overriding function Is_Expression_Element
    (Self : Base_Qualified_Expression)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      return True;
   end Is_Expression_Element;

   overriding procedure Visit
    (Self    : not null access Base_Qualified_Expression;
     Visitor : in out Program.Element_Visitors.Element_Visitor'Class) is
   begin
      Visitor.Qualified_Expression (Self);
   end Visit;

   overriding function To_Qualified_Expression_Text
    (Self : aliased in out Qualified_Expression)
      return Program.Elements.Qualified_Expressions
          .Qualified_Expression_Text_Access is
   begin
      return Self'Unchecked_Access;
   end To_Qualified_Expression_Text;

   overriding function To_Qualified_Expression_Text
    (Self : aliased in out Implicit_Qualified_Expression)
      return Program.Elements.Qualified_Expressions
          .Qualified_Expression_Text_Access is
      pragma Unreferenced (Self);
   begin
      return null;
   end To_Qualified_Expression_Text;

end Program.Nodes.Qualified_Expressions;
